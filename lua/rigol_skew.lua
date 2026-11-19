#!./lua.sh

-- Rigol DS1104Z edge skew, CH1 source, CH2 end of a delay chain
-- usage: cd /plots ; /exo/uc_tools/lua/lua.sh /exo/uc_tools/lua/rigol_skew.lua <pulse script> [shots]
-- - lua.sh needs ~/.result/lua: etc_net/nix/nix-build.sh lua
-- - probe calibration test: <pulse script> = 'echo 16', both probes on the cal output
--
-- Measurement
-- - one :SING per run of <pulse script>, trigger CH1 falling, 50us/div,
--   600k points at 500MSa/s: dmx break + zero byte fits, 4 edges per channel
-- - <pulse script> prints the 36MHz divisor on stdout (16 = 2.25M, 144 = 250k)
-- - each CH2 edge paired with the nearest CH1 edge in the same direction,
--   shot rejected unless the pairing is 1:1 and the same as the first shot's
-- - per edge: 50% crossing from a line fit over the 20-80% samples,
--   skew CH2-CH1; per low pulse: width per channel
-- - vertical scale and trigger level: by hand, signal should fill the screen
-- - probe skew: run with both probes on one point, subtract
--
-- Files into $PWD
-- - results.tsv: one row per shot (seconds), scope's own FDEL/RDEL as cross-check
-- - shot<N>.bmp: scope screen
-- - shot<N>_e<E>.bmp: edge plot from the raw data, scope style per div
-- - shot<N>_ch<C>.bin: raw bytes, volts = (byte - yorigin - yref) * yinc
--
-- Scope quirks (00.04.02.SP4)
-- - messages <200ms apart get garbled, valid commands now and then get
--   'command error': every command is acked with *OPC?, retried with back-off
-- - RAW preamble: points is the :WAV:STAR/STOP range, xorigin is off
-- - zoom on a stopped capture shows screen data only: plots are drawn here
--
-- - scope: RIGOL=<ip>, lluv does not resolve names
-- - actor task on lib/actor_uv.lua spawn_tcp_client, to be reused in c8

local actor    = require('lib.actor')
local actor_uv = require('lib.actor_uv')
local uv       = require('lluv')
local mixin    = require('lib.mixin')

local function log(str) io.stderr:write(str) end
local function logf(...) log(string.format(...)) end

local SCALE = 50e-6     -- 600us window, trigger 1 div from the left
local MDEP  = 600000    -- keeps 500MSa/s with 2 channels
local CHUNK = 100000    -- :WAV:DATA? points per read
local TO_MS = 5000

-- SCPI behavior, mixed into the connection task
-- - raw mode: socket chunks {socket, data} in the mailbox, self.rx holds the rest
local scpi = {}

function scpi:fill(ms)
   local msg = self:recv_with_timeout(
      ms or TO_MS, function(m) return type(m) == 'table' and m[1] == self.socket end)
   if msg[1] ~= self.socket then error('scpi: timeout') end
   self.rx = self.rx .. msg[2]
   -- - garbled input: reply without newline
   if self.rx:find('command error', 1, true) then error('scpi: command error') end
end
function scpi:read_bytes(n)
   while #self.rx < n do self:fill() end
   local data = self.rx:sub(1, n)
   self.rx = self.rx:sub(n + 1)
   return data
end
function scpi:read_line(ms)
   while not self.rx:find('\n') do self:fill(ms) end
   local i = self.rx:find('\n')
   return self:read_bytes(i):sub(1, -2)
end
-- - no ack on commands: *OPC? as one
-- - <200ms between messages and the scope can garble them, as rigol.erl
-- - not :send(), that is the actor mailbox
function scpi:put(str)
   self:write_socket(self.socket, str .. '\n')
   self:sleep(200)
end
function scpi:cmd(str)
   self:retry(str, function() self:put(str) ; self:put('*OPC?') ; assert(self:read_line() == '1') end)
end
-- - the scope drops requests now and then, or replies 'command
--   error' to a valid one: back off, flush and retry
function scpi:retry(what, f)
   for _ = 1, 10 do
      local ok, rv = pcall(f)
      if ok then return rv end
      logf('scpi: %s: %s, retry\n', what, rv)
      self:sleep(500)
      self.rx = ''
      while #self.mbox > 0 do table.remove(self.mbox) end
   end
   error('scpi: giving up: ' .. what)
end
function scpi:line(query)
   return self:retry(query, function() self:put(query) ; return self:read_line() end)
end
function scpi:number(query)
   return tonumber(self:line(query))
end
-- - #<n><len><data>\n, optional expected len
-- - setup(), optional, inside the retry: settings the scope can drop
--   although *OPC? acks them, e.g. :WAV:STAR/STOP
function scpi:block(query, len, setup)
   return self:retry(
      query, function()
         if setup then setup() end
         self:put(query)
         while self:read_bytes(1) ~= '#' do end
         local n = tonumber(self:read_bytes(1))
         local data = self:read_bytes(tonumber(self:read_bytes(n)))
         self:read_line()
         assert(not len or #data == len, 'short block ' .. #data .. '/' .. tostring(len))
         return data
      end)
end

local function write_file(name, data)
   local f = assert(io.open(name, 'wb'))
   f:write(data)
   f:close()
end

-- 50% crossings, 10% hysteresis
-- - levels: 5th / 95th percentile, from a byte histogram
-- - crossing: least squares line through the 20-80% samples around
--   it, solved for 50%: sub-sample on slow edges, where 2-point
--   interpolation only sees 8 bit steps
-- - {{'fall' | 'rise', x, k0, k1}}: 0-based sample index, fit range; mid
local function crossings(s)
   local function v(k) return s:byte(k + 1) end
   local hist = {}
   for x = 0, 255 do hist[x] = 0 end
   for k = 0, #s - 1 do local x = v(k) ; hist[x] = hist[x] + 1 end
   local function pct(p)
      local acc = 0
      for x = 0, 255 do
         acc = acc + hist[x]
         if acc >= p * #s then return x end
      end
   end
   local lo, hi = pct(0.05), pct(0.95)
   local mid, h = (lo + hi) / 2, (hi - lo) / 10
   local b0, b1 = lo + 2 * h, hi - 2 * h
   local function inband(k) local x = v(k) ; return x > b0 and x < b1 end
   local edges = {}
   local high = v(0) >= mid
   local last = 0
   for k = 0, #s - 1 do
      local x = v(k)
      if high and x < mid - h or not high and x > mid + h then
         -- - fit range: last old side sample, walked out through the band
         local k0, k1 = last, last + 1
         while k0 > 0 and inband(k0 - 1) do k0 = k0 - 1 end
         while k1 < #s - 1 and inband(k1 + 1) do k1 = k1 + 1 end
         local n, sk, sv, skk, skv = 0, 0, 0, 0, 0
         for j = k0, k1 do
            local y = v(j)
            n, sk, sv, skk, skv = n + 1, sk + j, sv + y, skk + j * j, skv + j * y
         end
         local slope = (n * skv - sk * sv) / (n * skk - sk * sk)
         local xc
         if slope == slope and slope ~= 0 then
            xc = (mid - (sv - slope * sk) / n) / slope
         else
            local a, b = v(last), v(last + 1)
            xc = last + (a - mid) / (a - b)
         end
         table.insert(edges, { high and 'fall' or 'rise', xc, k0, k1 })
         high = not high
         last = k
      elseif (x >= mid) == high then
         last = k
      end
   end
   return edges, mid
end

-- 8x16 font from ../mod_vga_font_8x16.c, MSB left
local font = {}
do
   local dir = (arg[0]:match('(.*)/') or '.')
   local f = assert(io.open(dir .. '/../mod_vga_font_8x16.c'))
   local bytes = {}
   for line in f:lines() do
      if line:find('/%* glyph') then
         for h in line:match('^(.-)/%*'):gmatch('0x(%x%x)') do table.insert(bytes, tonumber(h, 16)) end
      end
   end
   f:close()
   for c = 0, 255 do font[c] = { unpack(bytes, c * 16 + 1, c * 16 + 16) } end
end

-- smallest 1-2-5 step >= x
local function step125(x)
   local e = 10 ^ math.floor(math.log10(x))
   for _, m in ipairs({ 1, 2, 5, 10 }) do
      if m * e >= x * (1 - 1e-9) then return m * e end
   end
end
local function si(x, unit)
   local a = math.abs(x)
   if a < 1e-15 then return '0' .. unit end
   for _, p in ipairs({ { 1, '' }, { 1e-3, 'm' }, { 1e-6, '\230' }, { 1e-9, 'n' } }) do
      if a >= p[1] * 0.999 or p[2] == 'n' then
         return (string.format('%.3g', x / p[1]) .. p[2] .. unit)
      end
   end
end

-- edge plot, 24 bit BMP, scope style
-- - 10 x 8 div, 1-2-5 per div, t = 0 at the CH1 50% crossing
-- - CH1 yellow, CH2 cyan, volts from the preamble, 50% levels dim,
--   crossings as vertical lines
-- - window: both fit ranges plus half their span
local W, H = 800, 480
local L, T, PW, PH = 72, 32, 700, 400   -- plot area, 70 x 50 px per div
local function plot(name, title, s1, s2, e1, e2, mid1, mid2, pre1, pre2)
   local dt = pre1[5]
   local function volt(pre, b) return (b - pre[9] - pre[10]) * pre[8] end
   local x1, x2 = e1[2], e2[2]
   local a, b = math.min(e1[3], e2[3], x1, x2), math.max(e1[4], e2[4], x1, x2)
   local margin = math.max(20, (b - a) / 2)
   a, b = a - margin, b + margin
   -- - time: relative to x1, grid on round times
   local tdiv = step125((b - a) * dt / 10)
   local t0 = (math.floor(((a + b) / 2 - x1) * dt / tdiv - 5 + 0.5)) * tdiv
   local ia, ib = math.max(0, math.floor(x1 + t0 / dt)), math.ceil(x1 + (t0 + 10 * tdiv) / dt)
   -- - volts: both channels, 8 div
   local vmin, vmax = math.huge, -math.huge
   for _, c in ipairs({ { s1, pre1 }, { s2, pre2 } }) do
      for i = ia, math.min(ib, #c[1] - 1) do
         local v = volt(c[2], c[1]:byte(i + 1))
         vmin, vmax = math.min(vmin, v), math.max(vmax, v)
      end
   end
   local vdiv = step125(math.max(vmax - vmin, 1e-3) / 7)
   local v0 = math.floor((vmin + vmax) / 2 / vdiv - 4 + 0.5) * vdiv
   if v0 > vmin then v0 = v0 - vdiv end
   local px = {}
   local function set(x, y, c)
      if x >= 0 and x < W and y >= 0 and y < H then px[y * W + x] = c end
   end
   local function plotset(x, y, c)
      if x >= L and x < L + PW and y > T and y <= T + PH then set(x, y, c) end
   end
   -- - y down from the top here, flipped on output
   local function X(t) return math.floor(L + (t - t0) / (10 * tdiv) * PW + 0.5) end
   local function Y(v) return math.floor(T + PH - (v - v0) / (8 * vdiv) * PH + 0.5) end
   local function text(x, y, str, c)
      for n = 1, #str do
         local g = font[str:byte(n)]
         for r = 1, 16 do
            for k = 0, 7 do
               if math.floor(g[r] / 2 ^ (7 - k)) % 2 == 1 then set(x + (n - 1) * 8 + k, y + r - 1, c) end
            end
         end
      end
   end
   local grey, dark, white = '\96\96\96', '\48\48\48', '\200\200\200'
   -- - grid: div lines dotted, frame and center lines solid
   for d = 0, 10 do
      local x = L + d * PW / 10
      for y = T, T + PH do if d == 0 or d == 10 or d == 5 or y % 4 == 0 then set(x, y, (d == 0 or d == 10) and grey or dark) end end
      local lbl = si(t0 + d * tdiv, 's')
      text(x - 4 * #lbl, T + PH + 6, lbl, white)
   end
   for d = 0, 8 do
      local y = T + d * PH / 8
      for x = L, L + PW do if d == 0 or d == 8 or d == 4 or x % 4 == 0 then set(x, y, (d == 0 or d == 8) and grey or dark) end end
      local lbl = si(v0 + (8 - d) * vdiv, 'V')
      text(L - 6 - 8 * #lbl, y - 8, lbl, white)
   end
   text(L, 8, title, white)
   local foot = si(tdiv, 's') .. '/div  ' .. si(vdiv, 'V') .. '/div  t: from CH1 50%'
   text(L + PW - 8 * #foot, T + PH + 26, foot, white)
   text(L, T + PH + 26, 'CH1', '\0\255\255')
   text(L + 40, T + PH + 26, 'CH2', '\255\255\0')
   for ch, d in ipairs({ { s1, mid1, x1, pre1, '\0\255\255', '\0\96\96' },
                         { s2, mid2, x2, pre2, '\255\255\0', '\96\96\0' } }) do
      local s, mid, xc, pre, c, dim = unpack(d)
      for x = L, L + PW do plotset(x, Y(volt(pre, mid)), dim) end
      for y = T, T + PH do plotset(X((xc - x1) * dt), y, dim) end
      local dots = PW / ((ib - ia) + 1) >= 4
      local px0, py0
      for i = ia, math.min(ib, #s - 1) do
         local x, y = X((i - x1) * dt), Y(volt(pre, s:byte(i + 1)))
         if px0 then
            local n = math.max(math.abs(x - px0), math.abs(y - py0), 1)
            for k = 0, n do plotset(px0 + math.floor((x - px0) * k / n + 0.5), py0 + math.floor((y - py0) * k / n + 0.5), c) end
         end
         if dots then for dx = -1, 1 do for dy = -1, 1 do plotset(x + dx, y + dy, c) end end end
         px0, py0 = x, y
      end
   end
   -- - rows bottom-up, BGR, W * 3 is a multiple of 4
   local function u32(v) return string.char(v % 256, math.floor(v / 256) % 256, math.floor(v / 65536) % 256, math.floor(v / 16777216)) end
   local rows = {}
   for y = H - 1, 0, -1 do
      local row = {}
      for x = 0, W - 1 do row[x + 1] = px[y * W + x] or '\0\0\0' end
      rows[H - y] = table.concat(row)
   end
   write_file(name, table.concat({
      'BM', u32(54 + W * H * 3), u32(0), u32(54),
      u32(40), u32(W), u32(H), '\1\0\24\0', u32(0), u32(W * H * 3), u32(2835), u32(2835), u32(0), u32(0),
      table.concat(rows) }))
end

local skew = {}

function skew:setup()
   for _, c in ipairs({
         ':RUN',
         ':CHAN1:DISP ON', ':CHAN2:DISP ON', ':CHAN3:DISP OFF', ':CHAN4:DISP OFF',
         ':ACQ:TYPE NORM', ':ACQ:MDEP ' .. MDEP,
         ':TIM:DEL:ENAB OFF',
         ':TIM:MAIN:SCAL ' .. SCALE, ':TIM:MAIN:OFFS ' .. 5 * SCALE,
         ':TRIG:MODE EDGE', ':TRIG:EDG:SOUR CHAN1', ':TRIG:EDG:SLOP NEG',
         ':MEAS:CLE ALL',
         ':MEAS:SET:DSA CHAN1', ':MEAS:SET:DSB CHAN2',
         ':MEAS:ITEM FDEL', ':MEAS:ITEM RDEL' }) do
      self:cmd(c)
   end
end

-- - :TRIG:STAT? until one of states
function skew:wait_trig(states)
   local st
   for _ = 1, 50 do
      st = self:line(':TRIG:STAT?')
      if states[st] then return st end
      self:sleep(100)
   end
   error('trigger: ' .. st)
end

-- raw memory of one channel after :STOP
-- - preamble: format,type,points,count,xinc,xorig,xref,yinc,yorig,yref
-- - preamble points is the :WAV:STAR/STOP range, :ACQ:MDEP? is all of it
function skew:wav(ch, f)
   self:cmd(':WAV:SOUR CHAN' .. ch)
   self:cmd(':WAV:MODE RAW')
   self:cmd(':WAV:FORM BYTE')
   local pre = {}
   for x in self:line(':WAV:PRE?'):gmatch('[^,]+') do table.insert(pre, tonumber(x)) end
   local points = self:number(':ACQ:MDEP?')
   local chunks = {}
   for start = 1, points, CHUNK do
      local stop = math.min(start + CHUNK - 1, points)
      table.insert(chunks, self:block(':WAV:DATA?', stop - start + 1, function()
         self:cmd(':WAV:STAR ' .. start)
         self:cmd(':WAV:STOP ' .. stop)
      end))
   end
   local data = table.concat(chunks)
   write_file(f('_ch' .. ch .. '.bin'), data)
   return data, pre
end

function skew:shot(i)
   local function f(suffix) return 'shot' .. i .. suffix end
   self:cmd(':SING')
   -- - a free running signal (probe cal) triggers before the script runs
   self:wait_trig({ WAIT = true, STOP = true })
   local p = io.popen(self.script)
   local out = p:read('*a')
   p:close()
   local div = tonumber(out:match('(%d+)%s*$'))
   if not div then error('no divisor in: ' .. out) end
   self:wait_trig({ STOP = true })
   write_file(f('.bmp'), self:block(':DISP:DATA?'))
   -- - scope's own numbers, first edge pair only: cross-check
   local fdel, rdel = self:line(':MEAS:ITEM? FDEL'), self:line(':MEAS:ITEM? RDEL')
   local srat = self:number(':ACQ:SRAT?')
   local s1, pre1 = self:wav(1, f)
   local s2, pre2 = self:wav(2, f)
   local dt = pre1[5]
   local e1, mid1 = crossings(s1)
   local e2, mid2 = crossings(s2)
   local dirs = function(e)
      local d = {}
      for _, x in ipairs(e) do table.insert(d, x[1]) end
      return table.concat(d, ',')
   end
   -- - CH2 can miss edges, e.g. an undriven line until the chain
   --   drives it: pair each CH2 edge with the nearest CH1 edge in the
   --   same direction, n = CH1 edge numbers, increasing
   local n, a, b = {}, {}, {}
   for _, y in ipairs(e2) do
      local k
      for j, x in ipairs(e1) do
         if x[1] == y[1] and (not k or math.abs(x[2] - y[2]) < math.abs(e1[k][2] - y[2])) then k = j end
      end
      if not k or k <= (n[#n] or 0) then n = {} ; break end
      table.insert(n, k) ; table.insert(a, e1[k]) ; table.insert(b, y)
   end
   local sig = dirs(e1) .. ' ' .. table.concat(n, ',')
   if #n == 0 or (self.sig and self.sig ~= sig) then
      logf('shot %d: rejected, edges %s / %s\n', i, dirs(e1), dirs(e2))
      return
   end
   self.sig = sig
   local t1, t2, row, cols = {}, {}, {}, {}
   for p = 1, #n do
      -- - t = 0 at the trigger, the first CH1 edge: preamble xorigin is off in RAW
      t1[p] = (a[p][2] - e1[1][2]) * dt
      t2[p] = (b[p][2] - e1[1][2]) * dt
      table.insert(row, t2[p] - t1[p])
      table.insert(cols, 'skew_e' .. n[p] .. '_' .. a[p][1])
   end
   -- - low pulses: fall followed by rise
   for ch, t in ipairs({ t1, t2 }) do
      for p = 1, #n - 1 do
         if a[p][1] == 'fall' and n[p + 1] == n[p] + 1 then
            table.insert(row, t[p + 1] - t[p])
            table.insert(cols, 'low_e' .. n[p] .. '_ch' .. ch)
         end
      end
   end
   if not self.cols then
      self.cols = cols
      self.tsv:write(table.concat({ 'shot', 'divisor', 'srate', table.concat(cols, '\t'),
                                    'scope_fdel', 'scope_rdel' }, '\t') .. '\n')
   end
   self.tsv:write(table.concat({ i, div, srat, table.concat(row, '\t'), fdel, rdel }, '\t') .. '\n')
   self.tsv:flush()
   for p = 1, #n do
      plot(f('_e' .. n[p] .. '.bmp'),
           string.format('shot %d edge %d %s   CH2-CH1 %s', i, n[p], a[p][1], si(row[p], 's')),
           s1, s2, a[p], b[p], mid1, mid2, pre1, pre2)
   end
   local ns = {}
   for p = 1, #n do table.insert(ns, string.format('%.2f', row[p] * 1e9)) end
   logf('shot %d: div %d, %.0fMSa/s, skew ns %s\n', i, div, srat / 1e6, table.concat(ns, ' '))
   return row
end

function skew:summary(rows)
   logf('%d valid shots\n', #rows)
   if #rows == 0 then return end
   for c, name in ipairs(self.cols) do
      local sum = 0
      for _, r in ipairs(rows) do sum = sum + r[c] end
      local mean = sum / #rows
      local dev = 0
      for _, r in ipairs(rows) do dev = dev + (r[c] - mean) ^ 2 end
      logf('%-16s %12.2fns +- %.2fns\n', name, mean * 1e9, math.sqrt(dev / #rows) * 1e9)
   end
end

-- task body
-- - errors kill the task, not the uv loop: stop it here
function skew:connect()
   local ok, err = pcall(self.run, self)
   if not ok then log(tostring(err) .. '\n') end
   uv.stop()
end
function skew:run()
   self.rx = ''
   self:setup()
   self.tsv = assert(io.open('results.tsv', 'w'))
   local rows = {}
   for i = 1, self.shots do
      local row = self:shot(i)
      if row then table.insert(rows, row) end
   end
   self.tsv:close()
   self:summary(rows)
end

function skew:halt() uv.stop() end

local script, shots, ip = arg[1], tonumber(arg[2] or 10), os.getenv('RIGOL')
if not script or not ip then
   log('usage: RIGOL=<ip> rigol_skew.lua <pulse script> [shots]\n')
   os.exit(1)
end
local sched = actor.scheduler.new()
actor_uv.spawn_tcp_client(
   sched,
   { ip = ip, port = 5555, mode = 'raw',
     connection = function()
        local task = { script = script, shots = shots }
        mixin.add(task, skew)
        mixin.add(task, scpi)
        return task
     end })
uv.run()

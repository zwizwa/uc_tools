-- All generators are parameterized by a configuration environment
-- 'e'.  A single flat namespace is a little messy, but at the current
-- complexity level that seems to be the simplest way to do render
-- parameterization.

-- See notes at the bottom.

local list = require('lib.tools.list')


-- Modeled after elfutils/init.lua
-- See there about how to package as lua rock later.

-- Note that the path is a bit arbitrary, but it is done in a way that
-- will (eventually) be least amount of friction for a luarocks build
-- script.  For local development we can just symlink the .so to the
-- correct place.

local log_parse = require('lib.log_parse')


local logsvg = {}

local prompt = require('prompt')
local function log(str) io.stderr:write(str) end
local function log_desc(thing) log(prompt.describe(thing)) end



-- Wrap SVG header with style sheet around a group of elements.
function logsvg.svg(e, g)
   assert(e)
   assert(e.width)
   assert(e.height)
   assert(g)
   return
      {'svg',
       {xmlns='http://www.w3.org/2000/svg',
        width=e.width,
        height=e.height},{
          {'style',{},
           {".small { font-family: monospace; font-size: 10px }\n"}},
          {'g',{},g}}}
end

function logsvg.translate(x, y)
   return 'translate(' .. x .. ',' .. y .. ')'
end

-- text element for log entry at specific time
function logsvg.logentry(e, time, text)
   local y = time / e.ticks_per_pixel
   return
      {'text',
       {width=e.width,
        height='auto',
        transform=logsvg.translate(0,y),
        class='small',
        stroke='black'},
       {text}}
end

-- line from adjusted time (next to text) to actual time
function logsvg.timelink(e, adj_time, time)
   local x1 = e.x_actual   or -20
   local x2 = e.x_adjusted or   0
   local y1 = time / e.ticks_per_pixel
   local y2 = adj_time / e.ticks_per_pixel
   return {'line',
           {height='auto',
            line='auto',
            x1=x1, y1=y1,
            x2=x2, y2=y2,
            stroke='black'}}
end


-- Merge logs, sort by time, add tag
function logsvg.merge_logs(logs, sort_by)
   sort_by = sort_by or 1
   local tagged_log = {}
   for i,log in ipairs(logs) do
      for j,entry in ipairs(log) do
         local time, adj_time, line = unpack(entry)
         -- log_desc(log)
         table.insert(tagged_log, {time, adj_time, line, i})
      end
   end
   table.sort(tagged_log,
              function(a,b)
                 -- log_desc({a,b})
                 return a[sort_by] < b[sort_by] end)
   return tagged_log
end

function logsvg.render(e, logs)
   -- Repel operates on the individual logs
   assert(e.repel)
   assert(e.ticks_per_pixel)
   local repelled_logs = list.map(
      function(l)
         -- repel operates on time coordinates
         return logsvg.repel(l, e.repel * e.ticks_per_pixel) end,
      logs)
   -- For rendering, we need a single time stream to be able to do
   -- some time cuts.
   local y = 20
   local last_adj_time = 0
   -- Sort by adj_time, which is actual y distance used below for y_diff
   local sort_by = 2
   local merged_log = logsvg.merge_logs(repelled_logs, sort_by)

   local function render_g(e)
      local group_elements = {}
      for i, entry in ipairs(merged_log) do
         local time, adj_time, text, column = unpack(entry)
         local x = (column-1) * e.pixels_per_column
         local t_diff = adj_time - last_adj_time
         local y_diff = t_diff / e.ticks_per_pixel
         last_adj_time = adj_time

         -- Cut y space if there is too much time between subsequent
         -- log entries.  FIXME: Make this configurable.
         local y_diff_max = 70

         if (y_diff > y_diff_max) then
            local y_cut = y_diff - y_diff_max
            local t_cut = e.ticks_per_pixel * y_cut

            y = y - y_cut
            -- we add this to group_elements, which has absolute y
            -- coordinates, outside of transform for the entry
            local red_line_y_spacing = 15
            local y0 = adj_time / e.ticks_per_pixel + y - red_line_y_spacing
            table.insert(
               group_elements,
               {'g', {transform=logsvg.translate(0, y0)},
                {{'text',
                  {width='auto',  height='auto',
                   class='small', stroke='red'},
                  {"t_cut = " .. t_cut / 72000}},
                 {'line',
                  {height='auto', width='auto',
                   x1=0, y1=0, x2=e.width, y2=0,
                   stroke='red'}}}})
         end

         table.insert(
            group_elements,
            {'g', {transform=logsvg.translate(x,y)},
             {logsvg.logentry(e, adj_time, text),
              logsvg.timelink(e, adj_time, time)}})


      end

      -- Translate the entire frame to make room for the time pointers
      -- which have negative x coordinates.
      return {{'g', {transform=logsvg.translate(30,0)},
               group_elements}}
   end

   local g = render_g(e)

   -- This likely means that the ping synchronization message is
   -- missing.  FIXME: Create a better error message.
   if #merged_log == 0 then
      log('WARNING: merged_log is empty. sync ping missing?')
   end

   local t_total = merged_log[#merged_log][2] -- adj_time field
   local y_total = t_total / e.ticks_per_pixel
   -- As a side effect of rendering, y contains the total y adjust
   e.height = y_total + 20 + y
   return logsvg.svg(e, g)
end


-- Let the log entries repel each other, by moving them forward in
-- time if they are too close.
function logsvg.repel(entries, delta_t)
   if #entries == 0 then return entries end
   local t = entries[1][1]
   out_entries = {}
   for i, entry in ipairs(entries) do
      local time, adj_time, text = unpack(entry)
      -- Update t for next entry depending on whether this one fits.
      -- FIXME: draw a line from actual time to text location.
      if time >= t then
         adj_time = time
         t = time + delta_t
      else
         adj_time = t
         t = t + delta_t
      end
      table.insert(out_entries, {time, adj_time, text})
   end
   return out_entries
end


-- Alternative implementation, using the log_parse C parser + memory
-- mapped files.
--
-- FIXME: Copy-pased from earlier implementation.  Both should
-- probably be refactored or old should be deleted.
function logsvg.read_log_parse(filename, config)
   if not config then config = {} end
   -- log_desc({logsvg_read_log_parse_config = config})
   -- the default "" matches any message
   local sync_re = config.sync_re or "" -- "^ping (.-)"
   local max_lines = config.max_lines or 1000
   local bin_to_string = config.bin_to_string
   local lines = {}
   local last = nil
   local fist = nil
   local wraps = 0;

   local nxt = log_parse.next_ts_string
   -- If there is a binary to string converter supplied, we can use
   -- the iterator that doesn't convert the binaries to hex.
   if bin_to_string then nxt = log_parse.next_ts_raw end
   assert(nxt)

   -- In addition, allow user to override the iterator altogether.
   local sequence
   if config.sequence then
      sequence = config.sequence(filename, config)
   else
      sequence = config.sequence or
         log_parse.messages(
            {file = filename,
             -- The default {} does not perform any winding in C code.
             wind = config.wind or {}, -- {0},
             next = nxt,
             dir = config.dir })
   end

   -- FIXME: Let the C code do the scanning.
   for n, logline, is_bin in sequence do

      -- User can plug in binary log message parser
      if is_bin and bin_to_string then
         logline = bin_to_string(logline)
      end

      if max_lines and #lines > max_lines then return lines end
      -- log_desc({n = n, logline = logline})
      assert(n)
      assert(logline)
      if not last then
         if string.match(logline, sync_re) then
            log("sync:" .. n .. ":" .. filename .. "\n")
            last  = n
            first = n
         end
      end
      if last then
         local diff = n - last
         if (diff < 0) then
            wraps = wraps + 1
         end
         local adjusted_n = n - first + wraps * 0x100000000
         last = n

         local keep = true
         if config.drop_res then
            for _,re in ipairs(config.drop_res) do
               if string.match(logline, re) then
                  keep = false
                  break
               end
            end
         end
         if keep then
            table.insert(lines, {adjusted_n, adjusted_n, logline})
         end
      end
   end

   return lines
end


-- Read a time-stamped log.  For now this assumes that the timestamp
-- is a 8-digit lower case hex number at the start of the line,
-- followed by a space.
--
-- FIXME: To handle counter wrap-around the following convention is
-- used: it is assumed that there is at least one message per
-- wrap-around (32bit, 72MHz) such that there is no aliasing.  In that
-- case, we can just catch wraps here.

function logsvg.read_log(filename, config)

   if not config then config = {} end
   local sync_re = config.sync_re or "^ping (.-)"
   local max_lines = config.max_lines or 1000
   local lines = {}
   local last = nil
   local fist = nil
   local wraps = 0;

   for str in io.lines(filename) do
      --log("line: " .. str .."\n")

      local stamp, logline = string.match(str, "([0123456789abcdef]-) (.*)")

      --log("stamp: " .. stamp .. "\n")
      --log("logline: " .. logline .."\n")

      -- By default we have a limit.  Generating large SVGs doesn't
      -- work well, and is impossible to read anyway.  What this needs
      -- is skip and range, or some trigger mechanism + range.
      if max_lines and #lines > max_lines then return lines end

      local n = tonumber(stamp,16)
      if not (stamp and logline and n) then
         -- It's better to make this robust: reuse the previous time
         -- stamp if there is one, and copy the entire line.
         -- log("bad log line: " .. str .. "\n")
         n = last or 0
         logline = str
      end

      -- FIXME: shouldn't this be first?  it does seem to work though...
      if not last then
         if string.match(logline, sync_re) then
            log("sync:" .. stamp .. ":" .. filename .. "\n")
            last  = n
            first = n
         end
      end
      if last then
         local diff = n - last
         if (diff < 0) then
            wraps = wraps + 1
         end
         local adjusted_n = n - first + wraps * 0x100000000
         last = n
         table.insert(lines, {adjusted_n, adjusted_n, logline})
      end
   end
   return lines
end


-- For convenience.  This is the "user scenario": convert a list of
-- microcontroller trace log files to an svg.
function logsvg.render_logfiles(e, filenames)
   assert(e.repel)
   local function process(filename)
      -- Just re-use the environemnt object for parser config as well.
      local config = e
      local l = logsvg.read_log_parse(filename, config)
      return logsvg.repel(l, e.repel)
   end

   return logsvg.render(e, list.map(process, filenames))
end


return logsvg


-- Notes
--
-- 1. After first iteration, it seems a useful feature would be to
-- snap the dead space.  To do this, it might be simplest to
-- interleave the logs into a single one.

-- All generators are parameterized by a configuration environment
-- 'e'.  A single flat namespace is a little messy, but at the current
-- complexity level that seems to be the simplest way to do render
-- parameterization.

-- See notes at the bottom.

local list = require('lib.tools.list')

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
   assert(#merged_log > 0)

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


-- Put this in a tools library, together with the popen variant.
local function read_file(filename)
   local f = io.open(filename, "r+")
   if not f then
      error("cant_open:" .. filename)
   end
   local str = f:read('*a')
   f:close()
   return str
end


-- Read a time-stamped log.  For now this assumes that the timestamp
-- is a 8-digit lower case hex number at the start of the line,
-- followed by a space.
--
-- FIXME: To handle counter wrap-around the following convention is
-- used: it is assumed that there is at least one message per
-- wrap-around (32bit, 72MHz) such that there is no aliasing.  In that
-- case, we can just catch wraps here.

function logsvg.read_log(filename, sync_re)
   sync_re = sync_re or "^ping (.-)"
   local str = read_file(filename)
   local lines = {}
   local last = nil
   local fist = nil
   local wraps = 0;

   for stamp, logline in string.gmatch(str, "([0123456789abcdef]-) (.-)\n") do
      -- log(n .. "\n")
      -- log(logline .. "\n")
      local n = tonumber(stamp,16)
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
      local l = logsvg.read_log(filename)
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

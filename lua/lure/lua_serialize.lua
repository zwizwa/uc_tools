-- Attempt at replacing prompt.describe.
-- For printable objects, eval . print = id
-- See lua/lure/test_lua_serialize.lua

local m = {}

-- FIXME: This should only use lure. modules
local list = require('lib.tools.list')

m.unprintable = {
   ['loop']     = '#<loop>',
   ['function'] = '#<function>',
   ['thread']   = '#<thread>',
   ['userdata'] = '#<userdata>',
}

function m.make_serializer(cfg)
   local log = cfg.log ; assert(log)
   -- This allows caller to add an error() here if data is supposed
   local unprintable = cfg.unprintable or function(tag, obj) log(obj) end
   local key_indent = cfg.key_indent

   local function indent(level)
      if key_indent then
         log("\n")
         for i=1,level do log(key_indent) end
      end
   end

   return function(obj)
      local visited = {}
      local fmt
      local function log_obj(obj, level)
         local f = fmt[type(obj)]
         if f then return f(obj, level) end
         log(obj)
      end

      fmt = {
         ['boolean'] = function(obj, level) log(tostring(obj)) end,
         ['string'] = function(obj, level) log(string.format("%q",obj)) end,
         ['table'] = function(obj,level)
            assert(level)
            if (visited[obj]) then
               unprintable('loop')
               return
            end
            visited[obj] = true

            log("{ ")

            for k,v in list.sorted_pairs(obj) do
               indent(level)
               if type(k) == 'number' then
                  log('[')
                  log(tostring(k))
                  log(']')
               elseif type(k) == 'string' then
                  -- Just keep it simple: always quote.
                  log('[')
                  log(string.format("%q",k))
                  log(']')
               else
                  log_obj(k, level+1)
               end
               log(" = ")
               log_obj(v, level+1)
               log(", ")
            end
            indent(level-1)
            log("}")
         end,
         ['function'] = function(obj) unprintable('function',obj) end,
         ['userdata'] = function(obj) unprintable('userdata',obj) end,
         ['thread']   = function(obj) unprintable('thread',  obj) end,
      }

      log_obj(obj, 1)
      log("\n")

   end
end

function m.flat_logger(log)
   local cfg = {
      log = log,
      key_indent = nil,
   }
   return m.make_serializer(cfg)
end

function m.logger(log)
   local cfg = {
      log = log,
      key_indent = "  ",
   }
   return m.make_serializer(cfg)
end

function m.serialize(obj)
   local cfg = {}
   function cfg.unprintable(tag, thing)
      error('unprintable: ' .. tag)
   end
   local strs = {}
   local function append_strs(str)
      table.insert(strs, str)
   end
   local log_append = m.logger(append_strs)
   log_append(obj)
   return table.concat(strs,'')
end

return m

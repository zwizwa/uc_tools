#!./lua.sh

-- Test runner for property tests in qcprop.lua

local qcprop  = require('qcprop')
local qctools = require('lib.qctools')
require('lure.log')

local gen    = qctools.gen
local shrink = qctools.shrink

-- FIXME: Better bookkeeping
qcprop.heap = require('qcsm').heap


-- To implement private data only accessible by the test framework,
-- use a name that is not know to the prop functions.  Maybe change
-- this name from time to time to make sure it is really not accessed.
local priv = '_priv'

local tc = {}

-- All properties take tc as an environment object.  This contains a
-- 'c' member with functions from linux/mod_test_lua51.c that expose C
-- state machines and corresponding test routines.
tc.c = require('test_lua51')

function tc.new()
   local tc_state = {
      -- Private data.  Tests cannot access this
      [priv] = {
         seed     = tonumber(os.getenv("SEED") or "1"),
         max_size = tonumber(os.getenv("MAX_SIZE") or "30"),
         abort    = (tonumber(os.getenv("ABORT") or "0") > 0),
         loglevel = tonumber(os.getenv("LOGLEVEL") or "0"),
      },
   }
   setmetatable(tc_state, { __index = tc })
   return tc_state
end

-- Objects passed to the test functions have some restrictions.
function restricted(obj)
   local r_obj = {}
   local mtab = {}
   function mtab.__index(tab, key)
      local val = obj[key]
      if val ~= nil then return val end
      error("key '" .. key .. "' not defined")
   end
   setmetatable(r_obj, mtab)
   return r_obj
end
-- When logging is enabled, turn of restricted access.  This is mostly
-- to make log_desc work properly.  Tests are still mostly run with
-- loglevel == 0 so they have been executed with restriction enabled
-- so have been "type checked" in that sense.
function tc:restricted(obj)
   if self[priv].loglevel > 0 then
      return obj
   else
      return restricted(obj)
   end
end

function tc:log_desc(tab, loglevel)
   local loglevel = loglevel or self[priv].loglevel
   if loglevel > 0 then
      log_desc(tab)
   end
end

function tc:assert(bool)
   assert(bool)
end

function tc:error(msg)
   error(msg)
end

function tc:set_loglevel(loglevel)
   self[priv].loglevel = loglevel
end

function tc:random(gen, size)
   local val
   val, self[priv].seed = gen(self[priv].seed, size)
   return val
end

function tc:run_test(spec)
   local p = self[priv]
   p.name = spec[1]
   assert(p.name)

   -- Each test name indexes a pair containing type and property.
   local prop = qcprop[p.name]
   assert(type(prop) == 'table')
   -- FSM and pure tests are different
   if prop.init then
      return self:run_fsm_test(spec, prop)
   else
      return self:run_pure_test(spec, prop)
   end
end

-- Map exception to false.
local function e2f_pcall(...)
   local ok, rv = pcall(...)
   return ok and rv
end

function tc:run_fsm_test(spec, prop)
   local p = self[priv]
   -- log_desc(prop)

   -- Collect command names in a sorted list to remove dependency on
   -- the order of pairs() iterator.
   local cmd_names = {}
   assert(type(prop.cmd == 'table'))
   for cmd in pairs(prop.cmd) do
      table.insert(cmd_names, cmd)
   end
   table.sort(cmd_names)

   -- self:set_loglevel(1)  -- FIXME

   -- Unpack generator for the init function.
   local init_gen = gen.map(prop.init.typ(gen))
   local function gen_init()
      log(p.name .. " " .. p.size .. " " .. p.seed .. "\n")
      return self:random(init_gen, p.size)
   end

   -- Same for all command arguments
   local arg_gen= {}
   for _,cmd_name in ipairs(cmd_names) do
      local cmd = prop.cmd[cmd_name]
      local cmd_arg_gen = gen.map(cmd.typ(gen))
      arg_gen[cmd_name] = function() return self:random(cmd_arg_gen, p.size) end
   end

   local function run()
      -- Create initial state
      local init_args = gen_init()
      local ok, state = pcall(prop.init.app, self, init_args)
      assert(ok)
      -- Sequence a random number of state transition commands
      local nb_cmds = self:random(gen.range(1, p.size))
      -- log_desc({nb_cmds = nb_cmds})
      for cmd_nb=1,nb_cmds do
         -- Determine what are legal transitions in this state.
         local legal_names = {}
         for _,cmd in ipairs(cmd_names) do
            if prop.cmd[cmd].pre(state) then
               table.insert(legal_names, cmd)
            end
         end
         -- log_desc({legal_names=legal_names})
         assert(#legal_names > 0)
         -- Pick a random legal command
         local cmd_index = self:random(gen.range(1,#legal_names))
         local cmd_name = legal_names[cmd_index]
         -- Generate its argument list
         local cmd_arg = (arg_gen[cmd_name])()
         -- log_desc({cmd_arg=cmd_arg})
         -- Apply it to the state.  The app() function will modify
         -- state in-place and will return the result of all
         -- assertions.  Exceptions are mapped to failures.
         -- log_desc({cmd_name,cmd_arg})
         prop.cmd[cmd_name].app(state, cmd_arg)
         -- local ok = e2f_pcall(prop.cmd[cmd_name].app, state, cmd_arg)
         assert(ok)
         -- log_desc({queue=state.queue})
      end
   end

   if #spec == 1 then
      -- Generate a test configuration from arg_type
      p.size = 1
      while (p.size < p.max_size) do
         run()
         self:grow_size()
      end
   else
      p.size = spec[2]
      p.seed = spec[3]
      -- log_desc({size=p.size,seed=p.seed})
      run()
   end

end

-- Expected geometric growth
function tc:grow_size()
   local p = self[priv]
   local inc = self:random(gen.nat1, 1 + math.floor(p.size / 5))
   p.size = p.size + inc
end

function tc:run_pure_test(spec, prop)
   local p = self[priv]
   local r_tc = restricted(self)
   local config_type, eval_prop = unpack(prop)
   assert(type(config_type) == 'function')
   assert(type(eval_prop) == 'function')

   local function pcall_prop(cfg)
      local ok, rv = pcall(eval_prop, r_tc, cfg)
      if ok then
         -- Property evaluated properly
         return rv
      else
         -- When property raises an error, we treat it as if it
         -- evaluated to false.
         log_desc("pcall_prop error\n")
         return false
      end
   end

   -- Instantiate the type as generators
   local config_gens = config_type(gen)
   -- Convert map of generators to generator of map.
   local config_gen = gen.map(config_gens)
   -- Bind it to the seed in tc.
   local function gen_config()
      log(p.name .. " " .. p.size .. " " .. p.seed .. "\n")
      return self:random(config_gen, p.size)
   end

   local function run_config(config)
      self:log_desc({config=config})

      local ok, rv = pcall_prop(config)
      if not ok then
         log("-> FAIL\n")

         -- Log what got us here, including priv generator parameters.
         self:log_desc({failed_config=config}, 1)

         -- Evaluate the type declaration with 'shrink' as semantics,
         -- giving a map of shrinkers.
         local shrinker_map = config_type(shrink)

         -- Implicitly the config parameter is always a map, so apply
         -- that level of wrapping as well
         local shrink = shrink.map(shrinker_map)

         -- Attempt shrink
         config = qctools.run_shrink(shrink, pcall_prop, config)
         self:log_desc({shrunk_config=config}, 1)

         if p.abort then
            log("Re-running with logging enabled:\n")
            self:set_loglevel(1)
            local rv = eval_prop(r_tc, config)
            os.exit(1)
         end
      end

   end

   if #spec == 1 then
      -- Generate a test configuration from arg_type
      p.size = 0
      while (p.size < p.max_size) do
         run_config(gen_config())
         self:grow_size()
      end
   else
      p.size = spec[2]
      p.seed = spec[3]
      -- log_desc({size=p.size,seed=p.seed})
      run_config(gen_config())
   end
end

local tc_state = tc.new()

if #arg == 0 then
   -- No arguments: run all tests once
   for name,_ in pairs(qcprop) do
      tc_state:run_test({name})
   end
elseif #arg == 1 then
   tc_state:run_test({arg[1]})
else
   -- Perform a single test run
   local name = arg[1]
   local size = tonumber(arg[2])
   local seed = tonumber(arg[3] or "1")
   tc_state:run_test({name,size,seed})
end

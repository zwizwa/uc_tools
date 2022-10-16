#!./lua.sh

-- Test runner for property tests in qcprop.lua

local qcprop  = require('qcprop')
local qctools = require('lib.qctools')
require('lure.log')

local gen    = qctools.gen
local shrink = qctools.shrink

-- To implement private data only accessible by the test framework,
-- use a name that is not know to the prop functions.  Maybe change
-- this name from time to time to make sure it is really not accessed.
local priv = '_priv'


local tc = {}

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

   -- Access to test environment is always restricted.
   local r_tc = restricted(self)
   -- Each test name indexes a pair containing type and property.
   local config_type, eval_prop = unpack(qcprop[p.name])
   assert(type(config_type) == 'function')
   assert(type(eval_prop) == 'function')

   local function pcall_prop(cfg)
      local ok, rv = pcall(eval_prop, r_tc, cfg)
      if ok then return rv else return false end
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

   local function run_test(config)
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

      -- Expected geometric growth
      local inc = self:random(gen.nat1, 1 + math.floor(p.size / 5))
      p.size = p.size + inc

   end

   if #spec == 1 then
      -- Generate a test configuration from arg_type
      p.size = 0
      while (p.size < p.max_size) do
         run_test(gen_config())
      end
   else
      p.size = spec[2]
      p.seed = spec[3]
      -- log_desc({size=p.size,seed=p.seed})
      run_test(gen_config())
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

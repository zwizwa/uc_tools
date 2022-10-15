-- Part of uc_tools/lua
-- https://opensource.org/licenses/MIT
-- COPYRIGHT HOLDER = Tom Schouten
-- YEAR = 2022

require('lib.tools.log')

local m = { gen = {}, shrink = {} }

-- Generators
local gen = m.gen
setmetatable(
   gen,
   {
      __index =
         function(typs,typ)
            error("Unknown type: " .. typ)
         end
   })

-- The random number generator function is patched using mutation and
-- is assumed to be Lua-global.  It is a constant in all practical
-- applications, and passing this as a parameter is too much hassle.
function gen.random() error 'no rng' end

function gen.range(min_inc, max_inc)
   return function(seed, size)
      -- log_desc({min_inc=min_inc, max_inc=max_inc})
      local range = max_inc - min_inc + 1
      if range <= 0 then
         return min_inc, seed
      end
      local val_u32, new_seed = gen.random(seed)
      local val = val_u32 % range
      -- log_desc({nat=val,seed=seed})
      return min_inc + val, new_seed
   end
end
function gen.nat(seed, size)
   local g = gen.range(0, size)
   return g(seed, size)
end
local function fmap(fun, gen)
   return function(seed, size)
      local val, new_seed = gen(seed, size)
      return fun(val), new_seed
   end
end
gen.bool = fmap(function(x) return x == 1 end, gen.range(0, 1))
gen.nat1 = fmap(function(x) return x + 1 end, gen.nat)

function gen.list(gen_el)
   return function(seed, size)
      local lst = {}
      for i=1,size do
         local val, new_seed = gen_el(seed, size)
         seed = new_seed
         table.insert(lst, val)
      end
      return lst, seed
   end
end

-- FIXME: Also do sized.
function gen.size(seed, size)
   return size, seed
end




-- Shrinkers
local shrink = m.shrink

-- In Haskell QuickCheck, a shrinker takes a value and returns a list
-- of immediate shrinks of that value.
--
-- Since lua is strict, we (probably) need some form of delayed
-- evaluation to avoid data explosion.
--
-- We represent lists by visitors, i.e. loops over all possible
-- imediate shrinks.  Early return is implemented using Lua
-- exceptions.

local function for_none(f) end
local function dont_shrink(val) return for_none end
setmetatable(shrink, { __index = function(k,v) return dont_shrink end })

-- Try halving first, then try decrementing.
local function nat_shrink_to(min_val)
   return function(val)
      return function(visit_nat)
         if (val > min_val) then
            local v1 = min_val + math.floor((val - min_val) / 2)
            local v2 = val - 1
            -- log_desc({v1=v1,val=val})
            -- assert(v1 < val)
            visit_nat(v1)
            if v2  ~= v1 then visit_nat(v2) end
         end
      end
   end
end

shrink.nat  = nat_shrink_to(0)
shrink.nat1 = nat_shrink_to(1)


local function lst_replace(lst, index, new_el)
   local lst1 = {}
   for i, el in ipairs(lst) do
      if i == index then
         if new_el ~= nil then
            table.insert(lst1, new_el)
         end
      else
         table.insert(lst1, el)
      end
   end
   return lst1
end

function shrink.list(shrink_el)
   return function(lst)
      return function(visit_list)
         for i=1,#lst do
            visit_list(lst_replace(lst, i))
         end
         for i,el_to_shrink in ipairs(lst) do
            -- log_desc({el_to_shrink=el_to_shrink})
            local for_shrunk = shrink_el(el_to_shrink)
            for_shrunk(
               function(el_shrunk)
                  visit_list(lst_replace(lst, i, el_shrunk))
               end)
         end
      end
   end
end

function shrink.map(shrink_els)
   -- log_desc({shrink_els = shrink_els})
   return function(map)
      -- log_desc({shrink_map = map})
      return function(visit_map)
         -- assert(type(f) == 'function')
         for key,shrink_el in pairs(shrink_els) do
            local for_map_el_shrinks = shrink_el(map[key])
            for_map_el_shrinks(
               function(shrunk_el)
                  local map1 = {}
                  for k in pairs(shrink_els) do map1[k] = map[k] end
                  -- log_desc({shrunk_el=shrunk_el})
                  map1[key] = shrunk_el
                  visit_map(map1)
               end)
         end
      end
   end
end

-- Perform shrink using the visitor-style shrinkers.
function m.run_shrink(shrinker, prop, args)
   -- log_desc({shrinking = args})
   local function done()
      -- log_desc({shrunk_to = args})
      return args
   end

   while true do
      local for_top_shrinks = shrinker(args)

      local nb_shrinks = 0
      local smaller = nil
      pcall(
         for_top_shrinks,
         function(shrunk)
            nb_shrinks = nb_shrinks + 1
            local pass = prop(args)
            if not pass then
               -- Found a new counterexample.
               smaller = shrunk
               error("stop")
            else
               -- log_desc({no_fail_shrunk=shrunk})
            end
         end)

      -- No shrinking happened
      if nb_shrinks == 0 then return done() end

      -- Shrinking happened, but property held for all.
      if smaller == nil then return done() end

      -- Actual shrink happened, try again.
      -- log_desc({smaller=smaller})
      args = smaller

   end
end




return m

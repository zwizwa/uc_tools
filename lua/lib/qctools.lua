-- Part of uc_tools/lua
-- https://opensource.org/licenses/MIT
-- COPYRIGHT HOLDER = Tom Schouten
-- YEAR = 2022

require('lib.tools.log')

local m = { gen = {}, shrink = {}, lib = {} }

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

-- FIXME: Add set_rng method instead of patching this default.
-- This is a Lua clone of xorshift.h srandom_u32
local function xorshift_srandom_u32(seed)
   local function trunc(x) return bit.band(x, 0xFFFFFFFF) end
   seed = trunc(bit.bxor(seed, bit.lshift(seed, 13)))
   seed = trunc(bit.bxor(seed, bit.rshift(seed, 17)))
   seed = trunc(bit.bxor(seed, (bit.lshift(seed, 5))))
   if (seed < 0) then seed = seed + 0x100000000 end
   return seed
end
m.lib.random = xorshift_srandom_u32
function gen.random(seed)
   -- error 'no rng'
   local val = xorshift_srandom_u32(seed)
   return val, val
end

function gen.range(min_inc, max_inc)
   return function(seed, size_not_used)
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


function gen.upto(max)
   return function (seed, size)
      local g = gen.range(0, math.min(size, max))
      return g(seed, size)
   end
end

function gen.size_then_upto(max)
   return function (seed, size)
      if (size <= max) then
         return size, seed
      else
         local g = gen.range(0, math.min(size, max))
         return g(seed, size)
      end
   end
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

-- Infinite stream.  This splits the random number generator, which we
-- implement by just duplicating the seed.  Typically this is used to
-- combine one finite data structure and mutilate it with an infinite
-- stream.  Which is usually just a sign that the data structure is
-- not abstracted well.
function gen.stream(gen_el)
   return function(seed, size)
      local function stream()
         local val
         val, seed = gen_el(seed, size)
         return val
      end
      return stream, seed
   end
end


-- Optionally a 'keys' argument specified the order of generation to
-- remove dependency on implementation-dependent order of the pairs()
-- iterator.
function gen.map(gen_els, keys)
   if nil == keys then
      -- Re-generate it if not specified.
      keys = {}
      -- log_desc({gen_els=gen_els})
      for k in pairs(gen_els) do
         table.insert(keys, k)
      end
      table.sort(keys)
   end
   return function(seed, size)
      local tab = {}
      for _, key in ipairs(keys) do
         local gen = gen_els[key]
         local val, new_seed = gen(seed, size)
         seed = new_seed
         tab[key] = val
      end
      return tab, seed
   end
end

-- FIXME: Also do sized.
function gen.size(seed, size)
   return size, seed
end

-- Pick a generator from a list of generators.
function gen.choice(gens)
   return function(seed, size)
      local val_u32, new_seed = gen.random(seed)
      local index = 1 + val_u32 % #gens
      local gen = gens[index]
      return gen(new_seed, size)
   end
end

-- Generate a product, splitting the size randomly
function gen.split_size(gena, genb)
   return function(seed, size)
      local size_a, seed1 = (gen.range(0,size))(seed, size)
      local size_b = size - size_a
      local val_a, seed2 = gena(seed1, size_a)
      local val_b, seed3 = genb(seed2, size_b)
      return {val_a, val_b}, seed3
   end
end

-- Recursive types
function gen.rec(leaf_conses, rec_conses)
   return function(seed, size)
      -- Pick a random constructor, but if the size is zero pick only
      -- a non-recursive leaf constructor.
      local nb_conses = #leaf_conses + #rec_conses
      if size == 0 then nb_conses = #leaf_conses end
      local i, seed1 = (gen.range(1,nb_conses))(seed, size)
      local cons
      if i <= #leaf_conses then
         cons = leaf_conses[i]
      else
         -- Undo thunk wrapping for recursive constructors
         cons = rec_conses[i - #leaf_conses]()
      end
      -- Reduce the size randomly, ensuring reduction.
      local size_red, seed2 = (gen.range(0,size-1))(seed1, size)
      return cons(seed2, size_red)
   end
end

-- Custom generator
function gen.gen(gen_fun)
   return gen_fun
end


function gen.const(val)
   return function(seed, size)
      return val, seed
   end
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

shrink.upto = shrink.nat
shrink.size_then_upto = shrink.nat

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


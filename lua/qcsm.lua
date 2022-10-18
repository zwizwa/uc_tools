-- State machine properties. See qcrun.lua for the runner.

-- The point is to have QuickCheck generate a random sequence of
-- commands while making sure all invariants hold.

-- Basic idea:
-- . A FSM is an object with a typed init function.
-- . Runner will instantiate init function with random args.
-- . A FSM api is a collection of typed transition functions (commands)
-- . Each command has a precondition to indicate if it is valid in a particular state
-- . Runner will pick a random sequence of commands based, using precondition to see what is valid

-- All the verification logic is baked into the app routine.
-- E.g. app does the bookkeping to predict SUT's behavor and compare results and/or state


-- Example: number heap (see mod_test_heap.c).
--
-- Model it using a sorted Lua list.

-- heap.init.typ: Type of initialization function.
-- heap.init.app: Initialization function.  The 'env' provides access to SUT etc...
-- heap.cmd.X.typ: Type of command
-- heap.cmd.X.pre: Is particular command valid in this state
-- heap.cmd.X.app: Perform transition on model and SUT, perform asserts, return pass/fail bool.

local m = {}

-- A test is an init function and a collection of commands descriptions.
local heap = { init = {}, cmd = { insert = {}, pop = {} } }

-- Init
function heap.init.typ(t) return { max_size = t.nat1 } end
function heap.init.app(tc, arg)
   return { queue = {}, tc = tc, max_size = arg.max_size,
            heap = tc.c.num_heap_a_new() }
end
-- Sorted insert
function heap.cmd.insert.pre(s) return #s.queue < s.max_size end
function heap.cmd.insert.typ(t) return { number = t.nat } end
function heap.cmd.insert.app(s,arg)
   table.insert(s.queue, arg.number) ; table.sort(s.queue)
   s.tc.c.num_heap_insert(s.heap, arg.number)
   s.tc:log_desc({'insert',arg.number},1)
   return true  -- FIXME: Anything to check here?
end
-- Pop top element
function heap.cmd.pop.pre(s) return #s.queue > 0 end
function heap.cmd.pop.typ(t) return {} end
function heap.cmd.pop.app(s)
   local exp = table.remove(s.queue)
   local sut = s.tc.c.num_heap_pop(s.heap)
   s.tc:log_desc({'pop',{exp=exp,sut=sut}}, 1)
   return exp == sut
end

m.heap = heap

return m


-- REVIEW LATER


--
-- Design:
--
-- . Instead of using a model, roll the prediction and SUT access and
--   pre/post condition evaluation into an abstract operation.
--
-- . The precondition is only needed for test case filtering.



-- Some notes:
--
-- . Started out with the question: "how is this more than state
--   comparison?", but then realized that this probably should work
--   also for opaque testing, where state is not available.
--
-- . The point is to test an API, meaning it is not always possible to
--   perform assertions on the internal state of the SUT, and what we
--   do is to try to predict what the API is doing.
--
-- . However, when access to SUT state is available to inspect we can
--   perform additional comparisons.  Question is: should the
--   predictor be interwoven with the SUT, or should we keep those
--   separate?
--
-- . So putting all that together, why not model only 2 components:
--   precondition used as filter, and apply which rolls everything
--   into one: model update, SUT update, model and SUT response and
--   state validation.
--
-- . Generating command sequences with a good distribution is not
--   easy!  E.g. outliers like 100x CMDA, 1xCMDB might be needed






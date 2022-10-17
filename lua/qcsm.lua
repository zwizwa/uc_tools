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


-- Example: Software timer test.  A sorted Lua list will do.  Note
-- that preconditions are fed to Lua assert() mostly to guard for
-- errors in the model.  The QCSM framework should not generate calls
-- with failing preconditions.

-- timer.init.typ: Type of initialization function.
-- timer.init.app: Initialization function.  The 'env' provides access to SUT etc...
-- timer.cmd.X.typ: Type of command
-- timer.cmd.X.pre: Is particular command valid in this state
-- timer.cmd.X.app: Perform transition on model and SUT, perform asserts, return pass/fail bool.

local m = {}

-- A test is an init function and a collection of commands descriptions.
local timer = { init = {}, cmd = { add = {}, pop = {} } }

-- Init
function timer.init.typ(t) return { max_size = t.nat1 } end
function timer.init.app(env, arg)
   return { queue = {}, env = env, max_size = arg.max_size }
end
-- Sorted insert
function timer.cmd.add.pre(s) return #s.queue < s.max_size end
function timer.cmd.add.typ(t) return { time = t.nat, event = t.nat} end
function timer.cmd.add.app(s,arg)
   table.insert(s.queue, {time=arg.time, event=arg.event})
   function less_than(e1, e2) return e1.time < e2.time end
   table.sort(s.queue, less_than)
   return true -- FIXME: apply to SUT and compare state
end
-- Pop top element
function timer.cmd.pop.pre(s) return #s.queue > 0 end
function timer.cmd.pop.typ(t) return {} end
function timer.cmd.pop.app(s)
   local el = table.remove(s.queue, 1)
   return true -- FIXME: apply to SUT and compare state
end

m.timer = timer

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






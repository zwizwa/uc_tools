
-- Regular expression matching on general sequences, meaning the the
-- string of symbols setting is generalized to a list of an arbitrary
-- algebraic data type, making it usable for structured log matching.
--
-- Split into these parts:
--
-- . Matcher backtracking algoritm implemented in terms of an NFA
--   representation of the regular language.  The string of input
--   elements is mapped to a string of primitive match results.
--
-- . A compiler that converts a regular expression DSL to an NFA.
--
--

-- OLD COMMENTS NEED REVIEW
--
-- Now that I understand it, I of course think that the path I used to
-- get to understanding is very meaningful.  The vessel can be
-- discarded.
--
-- Terminology adapted from https://en.wikipedia.org/wiki/Regular_grammar
--
-- Some more ideas here: https://stackoverflow.com/questions/37956483/regex-backtracking
--
--
-- Implementation: What is the basic step?
--
-- Given the next terminal symbol (i.e. which primitive matcher was
-- applied) look up the set (array) of production rules we have to
-- evaluate.  Pick the first one and record the remaining choices in a
-- bactracking stack.  If there are no matches, try the next
-- backtracking choice while splitting off remaining choiches in the
-- current rule.
--
-- Don't worry about efficiency in initial implementation.  Get
-- something that works first, then tune the representation while
-- keeping it working.  Continuation points can be represented as a
-- triplet: (symbol_index, non_terminal, rule_index).
--
-- The regular grammar and (epsilon-)NFA forms are almost the same
-- structure if we allow A->eB transitions, so it's probably ok if I
-- mix terminology a bit.  (E.g. treat "automaton transition" the same
-- as "grammar production rule")
--
-- The conversion from regular expression to NFA is straightforward
-- but typically produces many trivial epsilon transitions (where an
-- epsilon is the only transition out of the first state and there is
-- only one epsilon transition into the econd state). Ryan mentions in
-- the video that there is a way to get rid of these but let's not
-- worry about it.  I found another video explaining a general epsilon
-- elimination technique but that is a lot more involved and doesn't
-- seem like a good path to take.  Instead, just support the epsilon
-- transitions in the interpreter, since they do not introduce
-- backtracking points anyway and are just one extra indirection.  If
-- a regex is just executed once after translation to NFA / grammar
-- and if the sequences are short it probably doesn't save much
-- anyway.
--
-- So basically, implement this with the understanding that what we
-- call rules A->aB can also be seen as NFA transitions A-a->B etc:
-- https://www.youtube.com/watch?v=VbR1mGdP99s
--
-- The equivalence is explained here:
-- https://www.youtube.com/watch?v=fQVZ3B7qi6w

-- TODO:
-- . rework to NFA explicitly (explicit final states are needed to compose NFAs)
-- . rewrite comments

-- Do I need to reimplement it?  I don't think so.  Use the NFA
-- representation (with explicit multiple end states) to do the
-- composition, then convert the final one to a single end state.


require('lib.tools.log')


local lib = {}

-- env:   data that remains constant during execution
-- state: matching algorithm traversal state
local function rules_match_step(env, state)
   local log_desc = env.log_desc or function() end
   log_desc({STEP=state})

   -- Obtain current symbol (= abstract type, we only observe this as
   -- a match/no_match later)
   local symbol_index = state.symbol_index  ; assert(symbol_index)
   local symbols      = env.symbols         ; assert(symbols)
   if symbol_index > #symbols then
      -- FIXME: If this state is an end state we're good.
      -- End needs to be matched explicitly.
      if env.final[state.nfa_state] then
         state.result = true
         return
      else
         error('extra symbols')
      end
   end
   local symbol = symbols[symbol_index] ; assert(symbol)

   -- Obtain state transition rule set for the current NFA state.
   local nfa_state_rules = env.rules[state.nfa_state]
   assert(#nfa_state_rules > 0)
   local rule_index = state.rule_index
   assert(rule_index <= #nfa_state_rules)
   local rule = nfa_state_rules[rule_index]
   -- Next NFA state and input symbol matcher
   local rule_nfa_state, rule_input = unpack(rule)
   assert(rule_nfa_state)

   local prim_match = false
   if rule_input then
      if type(rule_input) == 'function' then
         prim_match = rule_input
      else
         -- Allow indirection.  This makes it possible to print rules at
         -- the expense of an extra table indirection.
         prim_match = env.prim[rule_input]
         assert(type(prim_match) == 'function')
      end

      log_desc({SYMBOL = {
                   symbol_index = symbol_index,
                   nfa_state = nfa_state,
                   nfa_state_rules = nfa_state_rules}})
   end

   -- Save continuation
   --
   -- If there are more rules to evaluate for this NFA state after
   -- we have evaluated the current one, we need to save that rule as
   -- a backtracking point.
   if rule_index + 1 <= #nfa_state_rules then
      local continuation = {
         symbol_index = symbol_index,
         rule_index   = rule_index + 1,
         nfa_state = state.nfa_state
      }
      log_desc({SAVE_CONTINUATION=continuation})
      table.insert(state.continuations, continuation)
   end

   -- Implement backtracker
   --
   -- This is called in every failing path below.
   local function backtrack()
      if #state.continuations > 0 then
         -- Resume at last continuation.
         local continuation = table.remove(state.continuations)
         log_desc({BACKTRACK=continuation})
         state.symbol_index = continuation.symbol_index
         state.nfa_state    = continuation.nfa_state
         state.rule_index   = continuation.rule_index
      else
         -- No more paths to tray, we are done with no match.
         state.result = false
      end
   end

   -- Epsilon rules do not match anything
   if not prim_match then
      log_desc({EPSILON = {
                   symbol = symbol,
                   symbol_index = symbol_index,
      }})

      state.nfa_state = rule_nfa_state
      -- state.symbol_index Doesn't change
      state.rule_index   = 1
      return
   end


   -- Check for a match.
   --
   -- Note that symbols are opaque from the pov of the regular
   -- languages structure.  They do carry substructure that is passed
   -- onto the caller of the regular expression matcher (e.g. data
   -- contained in log statements of a specific type).
   local match, payload = prim_match(symbol)
   log_desc({MATCH = {
                rule_input = rule_input,
                match = match,
                payload = payload,
                symbol = symbol,
                symbol_index = symbol_index,
            }})

   if not match then
      -- No match.  Backtrack if possible.
      backtrack()
   else
      -- Save the payload for each symbol.  If later we have to
      -- backtrack the results will just be overwritten.  When there
      -- is a match we are guaranteed to have associated each symbol
      -- with a payload.
      state.payload[state.symbol_index] = payload

      -- We have a match and there is a next NFA state.  Descend
      -- into the rule tree.
      state.nfa_state = rule_nfa_state
      state.symbol_index = symbol_index + 1
      state.rule_index   = 1
   end
end

local function rules_match(env)
   local log_desc = env.log_desc or function() end
   log_desc({env=env})
   local state = {
      continuations = { },
      symbol_index  = 1,
      nfa_state     = 'S', -- start symbol
      rule_index    = 1,   -- next rule is first rule in the nfa_state's rule set (array)
      result        = nil, -- resuilt is nil (no result), true (match), false (no match)
      payload       = {},  -- result of matcher for each symbol
   }
   while state.result == nil do
      rules_match_step(env, state)
   end

   local result
   if state.result == true then
      assert(#state.payload == #env.symbols)
      result = {true, state.payload}
   else
      result = {false}
   end
   log_desc({RESULT=result})
   return unpack(result)
end
lib.rules_match = rules_match


-- Convert regular expression to grammar rules used in the matcher.
-- https://www.youtube.com/watch?v=VbR1mGdP99s
--
-- Generate anonymous NFA states as sequence s1,s2,s3,...
--
-- Note that this is simplest to by using an explicit NFA
-- implementation where terminal states are listed explicitly.

-- FIXME: I need to restart this.  Basic operations
-- . change above representation as {state, symbol}
-- . join 2 nfas assuming states have different names = guaranteed by make_state()
-- . concatenation: for all final states in nfa1 add an epsilon transition to start of nfa2


function lib.test_regex_to_rules()
   local last_state = 0
   local function make_state()
      last_state = last_state + 1
      return string.format("s%d", last_state)
   end
   -- Convert symbol to nfa.
   local function nfa_symbol(s)
      local start = make_state()
      local final = make_state()
      return {
         start = start,
         final = { final },
         rules = {
            [start] = {{final, s}},
            [final] = {}
         }
      }
   end
   local function nfa_concat(nfa1, nfa2)
      -- A set (list) of transitions, encoded as {state, symbol}
      local nfa = { rules = {}, final = {} }
      -- nfa start state is nfa1 start state
      nfa.start = nfa1.start
      -- nfa final states are nfa2 final states (copy)
      for _, state in ipairs(nfa2.final) do table.insert(nfa.final, state) end
      -- nfa rules is the union of nfa1, nfa2 rules. states are mutually exclusive, guaranteed by make_state()
      for state, rule in pairs(nfa1.rules) do nfa.rules[state] = rule end
      for state, rule in pairs(nfa2.rules) do nfa.rules[state] = rule end
      -- nfa1 end states will get an epsilon transition to nfa2 start state
      for _, state in ipairs(nfa1.final) do
         table.insert(nfa.rules[state], {nfa2.start})
      end

      return nfa
   end


   -- nfa_union
   -- New start state, epsilon to both start states.
   -- New end state, epsilon from all end states.

   -- nfa_star
   -- New end state, epsilon from all end state
   -- New start state, epsilon to old start state, epsilon to new end state

   -- concat and union can be generalized to multi-arg

   local a = nfa_symbol('a') ; log_desc({a=a})
   local b = nfa_symbol('b') ; log_desc({b=b})
   local ab = nfa_concat(a, b) ; log_desc({ab=ab})

end




-- Primitive matchers represent the regular language's terminal symbols.
local function prim(event)
   return function(entry)
      if entry.event == event then
         return true, entry.time
      else
         return false
      end
   end
end


function lib.test_env2()
   return {
      log_desc = log_desc,
      symbols = {
         {event = 'start', time = 1},
         {event = 'mid',   time = 2},
         {event = 'mid',   time = 3},
         {event = 'stop',  time = 4},
      },
      prim = {
         start = prim('start'),
         mid   = prim('mid'),
         stop  = prim('stop'),
      },
      rules = {
         S = {
            {'E', 'start'},
         },
         E = {
            -- Epsilon rule E->M
            {'M'},
         },
         M = {
            {'M', 'mid'},
            {'F', 'stop'},
         },
         F = {
         },
      },
      final = {
         F = true,
      },
   }
end


function lib.test()
   -- lib.rules_match(lib.test_env1())
   lib.rules_match(lib.test_env2()) -- TODO: change to NFA structure
   -- lib.test_regex_to_rules()  -- TODO: Continue dev and unify with backtracker
end

return lib

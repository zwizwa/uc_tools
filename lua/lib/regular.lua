-- Module implementing right regular expression matching on general
-- sequences, meaning the the string of symbols setting is generalized
-- to a list of an arbitrary algebraic data type.  This was developed
-- for structured log matching/parsing.
--
-- Split into these parts:
--
-- . core representation abstracts "terminal symbols" as primitive
--   matching functions producing additional results that will be
--   collected by the matcher.
--
-- . production rules defined explicitly as set of
--   non-terminal -> (terminal, non-terminal)
--                -> terminal
--                -> empty TODO
--
-- . backtracking algorithm implementing the non-terminal production
--   choice
--
-- . translation of regexp-like syntax to production rules TODO
--
-- Terminology adapted from https://en.wikipedia.org/wiki/Regular_grammar
--
-- Some more ideas here: https://stackoverflow.com/questions/37956483/regex-backtracking


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


require('lib.tools.log')


local lib = {}

-- env:   data that remains constant during execution
-- state: matching algorithm traversal state
local function regexp_match_step(env, state)
   local log_desc = env.log_desc or function() end
   log_desc({STEP=state})

   -- Obtain current symbol (= abstract type, we only observe this as
   -- a match/no_match later)
   local symbol_index = state.symbol_index  ; assert(symbol_index)
   local symbols      = env.symbols         ; assert(symbols)
   if symbol_index > #symbols then
      -- End needs to be matched explicitly.
      error('extra symbols')
   end
   local symbol = symbols[symbol_index] ; assert(symbol)

   -- Obtain rule set for the current non-terminal.
   local non_terminal_rules = env.rules[state.non_terminal]
   assert(#non_terminal_rules > 0)
   local rule_index = state.rule_index
   assert(rule_index <= #non_terminal_rules)
   local rule = non_terminal_rules[rule_index]
   local rule_terminal, rule_non_terminal = unpack(rule)
   assert(rule_terminal)
   local prim_match = env.prim[rule_terminal]
   assert(type(prim_match) == 'function')

   log_desc({SYMBOL = {
                symbol_index = symbol_index,
                non_terminal = non_terminal,
                non_terminal_rules = non_terminal_rules}})

   -- Save continuation
   --
   -- If there are more rules to evaluate for this non-terminal after
   -- we have evaluated the current one, we need to save that rule as
   -- a backtracking point.
   if rule_index + 1 <= #non_terminal_rules then
      local continuation = {
         symbol_index = symbol_index,
         rule_index   = rule_index + 1,
         non_terminal = state.non_terminal
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
         state.non_terminal = continuation.non_terminal
         state.rule_index   = continuation.rule_index
      else
         -- No more paths to tray, we are done with no match.
         state.result = false
      end
   end

   -- Check for a match.
   --
   -- Note that symbols are opaque from the pov of the regular
   -- languages structure.  They do carry substructure that is passed
   -- onto the caller of the regular expression matcher (e.g. data
   -- contained in log statements of a specific type).
   local match, payload = prim_match(symbol)
   log_desc({MATCH = {
                rule_terminal = rule_terminal,
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

      if nil == rule_non_terminal then
         -- Whe have a match and there is no next non-terminal: we are
         -- done if the sequence is done.  Otherwise we have a partial
         -- match (TODO: distinguish those).
         if state.symbol_index == #env.symbols then
            state.result = true
         else
            backtrack()
         end
      else
         -- We have a match and there is a next non-terminal.  Descend
         -- into the rule tree.
         state.non_terminal = rule_non_terminal
         state.symbol_index = symbol_index + 1
         state.rule_index   = 1
      end
   end
end

local function regexp_match(env)
   local log_desc = env.log_desc or function() end
   log_desc({env=env})
   local state = {
      continuations = { },
      symbol_index  = 1,
      non_terminal  = 'S', -- start symbol
      rule_index    = 1,   -- next rule is first rule in the non_terminal's rule set (array)
      result        = nil, -- resuilt is nil (no result), true (match), false (no match)
      payload       = {},  -- result of matcher for each symbol
   }
   while state.result == nil do
      regexp_match_step(env, state)
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


lib.regexp_match = regexp_match

function lib.test_env()
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
   return {
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
            {'start', 'M'},
         },
         M = {
            {'mid', 'M'},
            {'stop'},
         },
      }
   }
end
function lib.test()
   local env = lib.test_env()
   env.log_desc = log_desc
   lib.regexp_match(env)
end

return lib

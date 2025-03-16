-- Module implementing regular expression matching on general
-- sequences, e.g. with "symbols" generalized to "data type variant
-- matching".
--
-- Restricted to right regular grammars which correspond better to the
-- way log files are typically represented: a list from bottom to top
-- (left to right) with oldest entry at the top (left).
--
-- Split into these parts:
--
-- . core representation implements "terminal symbols" as primitive
--   matchers with side effect that is to be collected by matcher
--
-- . production rules defined explicitly as set of
--   non-terminal -> (terminal, non-terminal)
--                -> terminal
--                -> empty
--
-- . translation of regexp-like syntax to production rules
--
-- . backtracking algorithm implementing the non-terminal production
--   choice
--
-- Terminology adapted from https://en.wikipedia.org/wiki/Regular_grammar
--
-- Some more ideas here: https://stackoverflow.com/questions/37956483/regex-backtracking


-- Implementation: What is the basic step?
--
-- Given the next terminal symbol (i.e. which primitive matcher was
-- applied) find the set of production rules we have to evaluate.
-- Record this as a backtracking choice.  If there are no matches, try
-- the next backtracking choice.

-- Don't worry about efficiency in initial implementation.  Get
-- something that works first, then tune the representation while
-- keeping it working.  It seems that the evaluation can be
-- represented as a coordinate i,j over (entry, rule) arrays.

-- Backtracking suspension:
-- . loop coordinates: symbol index, non_terminal + rule index
-- . these can just be saved in a list


require('lib.tools.log')


local lib = {}

-- A continuation consists of
-- {non_terminal,s,r}
-- with s an index into symbols array and
-- with r an index into the non_terminal rules array
--
-- At a decision point, the current continuation is updated by
-- advancing.  If advancing is not possible, the continuation is
-- discarded and the previous contination is popped.  At at choice
-- point, the continuation of the choice not taken is pushed.

-- env:   data that remains constant during execution
-- state: iteration state
local function regexp_match_step(env, state)
   log_desc({STEP=state})

   -- Obtain current symbol
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

   log_desc({SYMBOL =
                {symbol_index = symbol_index,
                 non_terminal = non_terminal,
                 non_terminal_rules = non_terminal_rules}})

   -- Save continuation
   --
   -- If there are more rules to evaluate starting from the current
   -- non-terminal, we need to save that rule as a backtracking point
   -- to be resumed if the choice we make here does not lead to a full
   -- match.
   if rule_index + 1 <= #non_terminal_rules then
      logf("SAVE CONTINUATION\n")
      table.insert(state.continuations,
                   { symbol_index = symbol_index,
                     rule_index   = rule_index + 1,
                     non_terminal = state.non_terminal })
   else
      logf("SINGLE RULE\n")
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

   -- FIXME: This is wrong. Likely we need to save the continuation
   -- before this.

   if not match then
      logf("NO MATCH\n")
      -- No match.  Backtrack if possible.
      if #state.continuations > 0 then
         logf("BACKTRACK\n")
         local continuation = table.remove(state.continuations)
         log_desc({CONTINUATION=continuation})
         state.symbol_index = continuation.symbol_index
         state.non_terminal = continuation.non_terminal
         state.rule_index   = continuation.rule_index

      else
         logf("NO BACKTRACK\n")
         error('TODO')
      end
   else
      if nil == rule_non_terminal then
         -- Whe have a match and there is no next non-terminal: we are
         -- done if the sequence is done.  Otherwise we have a partial
         -- match (TODO: distinguish those).
         logf("DONE\n")
         state.done = true
         return
      else
         -- This rule matches the terminal symbol. 
         logf("MATCH MORE\n")

         -- Descend into the rule tree.
         state.non_terminal = rule_non_terminal
         state.symbol_index = symbol_index + 1
         state.rule_index   = 1
      end
   end
end

local function regexp_match(env)
   log_desc({env=env})
   local state = {
      continuations = { },
      symbol_index  = 1,
      non_terminal  = 'S', -- start symbol
      rule_index    = 1,   -- next rule is first rule in the non_terminal's rule set (array)
      done          = false,
   }
   while not state.done do
      regexp_match_step(env, state)
   end
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
   lib.regexp_match(env)
end

return lib

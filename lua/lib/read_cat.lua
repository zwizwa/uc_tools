-- Part of uc_tools/lua
-- https://opensource.org/licenses/MIT
-- COPYRIGHT HOLDER = Tom Schouten
-- YEAR = 2023

-- Reader for a nested concatenative language.

return function(maybe_cfg)

-- Config defaults
local cfg = maybe_cfg or {}
local wrap_code = cfg.code or function(code) return {tag='code', code=code} end
local wrap_data = cfg.data or function(data) return {tag='data', data=data} end
local log_desc  = cfg.log_desc or function() end

local m = {}

-- Iterator for characters in a string.
function m.characters(str)
   local i = 1
   local function next_char()
      if i > #str then return nil end
      local c = str:sub(i,i)
      i = i + 1
      return c
   end
   return next_char
end

-- Iterator like ipairs but without index
function m.elements(array)
   local i = 1
   local function next_el()
      if i > #array then return nil end
      local el = array[i]
      i = i + 1
      -- log_desc({el=el})
      return el
   end
   return next_el
end

function m.tokenize(next_char)
   local words = {}
   local chars = {}
   local function end_word(next_word)
      -- Terminate current word
      local word = table.concat(chars)
      if word ~= "" then
         table.insert(words, word)
      end
      chars = {}
      -- Optionally insert a control token word
      if nil ~= next_word then
         table.insert(words, next_word)
      end
   end

   while true do
      local c = next_char()
      -- log_desc({c=c})

      -- Skip comments
      if c == "#" then
         repeat c = next_char()
         until c == '\n' or c == nil
      end

      -- Expand control tokens, terminating word.
      if     c == "'" then  end_word('quote')
      elseif c == '(' then  end_word('begin')
      elseif c == ')' then  end_word('end')
      -- Terminate word and return at end of stream.
      elseif c == nil then  end_word() ; break
      -- Terminate word at whitespace
      elseif c == ' ' or c == '\t' or c == '\n' then
         end_word()
      -- Collect characters in a word
      else
         table.insert(chars, c)
      end
   end

   return words

end

function m.parse(next_word, need_end)
   local function need_next_word()
      local word = next_word()
      if word == nil then error('no quoted word') end
      return word
   end
   local syntax = {}
   local function insert(stx)
      table.insert(syntax, stx)
   end
   while true do
      local word = next_word()
      if word == nil then
         if need_end then
            -- Need 'end' to match 'begin'
            error('no end')
         end
         break
      end
      if     word == 'quote' then insert(wrap_data(need_next_word()))
      elseif word == 'begin' then insert(m.parse(next_word, true))
      elseif word == 'end'   then break
      else                        insert(word)
      end
   end
   return wrap_code(syntax)
end


return m

end

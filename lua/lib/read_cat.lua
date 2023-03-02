-- Reader for a nested concatenative language.  Based on arrays
-- instead of s-expressions.

-- Reader is split into tokenizer and parser, which makes it more
-- straightforward to express.

require('lure.log')

local function string_to_stream(str)
   local i = 1
   local function next_char()
      if i > #str then return nil end
      local c = str:sub(i,i)
      i = i + 1
      return c
   end
   return next_char
end

local function array_to_stream(array)
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

local function tokenize(next_char)
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

local function parse(next_word, need_end)
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
      if     word == 'quote' then insert({need_next_word()})
      elseif word == 'begin' then insert(parse(next_word, true))
      elseif word == 'end'   then break
      else                        insert(word)
      end
   end
   return syntax
end


local function read_string(str)
   return read_stream(string_to_stream(str))
end

return {
   string_to_stream = string_to_stream,
   array_to_stream = array_to_stream,
   tokenize = tokenize,
   parse = parse,
}

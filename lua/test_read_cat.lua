#!./lua.sh
require('lure.log')
local read_cat = require('lib.read_cat')
local function tokenize(str)
   local rv = read_cat.tokenize(read_cat.string_to_stream(str))
   log_desc({'tokenize',str,rv})
end

tokenize('123')
tokenize('123 ')
tokenize(' 123 ')
tokenize('   123   ')
tokenize('123 abc')
tokenize('123   abc')
tokenize('123 \n  abc')
tokenize('123 abc # comment')
tokenize('123 abc # comment\n def')
tokenize("'")
tokenize("''")
tokenize("'a")
tokenize("'a 123")
tokenize("(")
tokenize("()")
tokenize("(123)")

local function parse(words, will_error)
   local stream = read_cat.array_to_stream(words)
   local ok, rv = pcall(read_cat.parse, stream)
   log_desc({'parse',words,ok,rv})
end

parse({'quote', '123'})
parse({'quote', '123', 'abc'})
parse({'begin', '123', 'abc', 'end'})
parse({'begin', '123', 'abc', 'end', 'def'})
parse({'begin', '123'})
parse({'quote'})



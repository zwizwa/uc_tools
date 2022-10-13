-- Not sure what to do with this.

-- Basically I want a DSL to do multi-substrate interpolation.  I
-- don't want a lot of dependencies so was thinking about embedding it
-- in Lua, maybe with a Lure wrapper around it.

local accu = {
   -- Type
   function(t) -- Type semantics is curried
      return t:fun(
         {t:int, t:int},
         {t:int, t:int}
      )
   end,
   -- Value
   function(e) -- Value semantics is curried
      return
         function(s, i) -- Functions are multi in, multi out
            local s1 = e:add(s, i)
            return s1, s1
         end
   end
}


-- In dataflow, are fancy types really necessary?  I mean the
-- direction is always forward, so abstract interpretation can resolve
-- all the types as long as the input types are provided.


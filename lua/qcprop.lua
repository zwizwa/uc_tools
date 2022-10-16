local m = {}
m.plus_assoc = {
   function(t)
      return { a = t.nat, b = t.nat }
   end,
   function(env, arg)
      env:log_desc({arg=arg})
      return arg.a + arg.b == arg.b + arg.a
   end
}
return m

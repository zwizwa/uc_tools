local m = {}
m.assoc = {
   function(t)
      return { a = t.nat, b = t.nat }
   end,
   function(env, args)
      env:log_desc({args=args})
      return args.a + args.b == args.b + args.a
   end
}
return m

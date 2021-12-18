; s-expression syntax for Lua CPS DSL.
(loop
 (let*
     ((a (read chan1))
      (b (read chan1)))
   (write (+ a b))))






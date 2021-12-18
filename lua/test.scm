; s-expression syntax for Lua CPS DSL.
(loop
 (let*
     ((a (read chan1))
      (b (read chan1))
      (c (let* ((x 1)
                (y 2))
           (+ a b x y)))
      (d (let* ((k 4)
                (l 5))
           (+ c k l))))
   (write d)))






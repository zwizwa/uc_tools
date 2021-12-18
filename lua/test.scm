; s-expression syntax for Lua CPS DSL.
; this compiles donw to C sm.h macros
; (add 1 2)

;(let* ((a 123)
;       (b 345))
;      (add a b 1))

(loop
 (let*
     ((a
       (let* ((b (read chan1))
              (c (read chan1)))
         (add b c)))
      (d (read chan1))
      (e (let* ((x (add a 1))
                (y (add d 2)))
           (add x y)))
      (f (read chan1))
      (g (let* ((l 5))
           (add f l))))
   (send d)
   (send f)
   (send e)
   ))






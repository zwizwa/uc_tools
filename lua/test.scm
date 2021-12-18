; s-expression syntax for Lua CPS DSL.
; this compiles donw to C sm.h macros
; (+ 1 2)

;(let* ((a 123)
;       (b 345))
;      (+ a b 1))

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
   (write c)
   (write d1)
   ))






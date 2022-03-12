;; Each expression is evaluated separately in test_scheme_liveness.lua

;; Straight line, single refs
(let*
    ((a 1)
     (b 2)
     (c (+ a b)))
  c)

;; Straight line, double ref
(let*
    ((a 1)
     (c (+ a a)))
  c)

;; The conditional branch is the problematic one, where reference
;; counts split.  What needs to be verified here:
;;
;; - Lifetime of a is different in the two branches
;; - The initial rc of a is the max of the two
;; - The branch that doesn't use a has a free hint on entry

(let ((a 1))
  (if #t
      123
      (+ a a)))



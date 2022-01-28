(define (x a b) (add a b))
(define (y z q) (add z q))


;; Note that non-definitions are currently not handled properly for
;; hoas mode.
;;(add 1 2)
;;(x 1 (x 2 3))
;;
(define (test_if)  (let* ((a (if 1 2 3))) a))

;; \(define (test_anf) (add 1 (add 2 3)))

(define (lala) (x 1 2))

(define (test_assign a) (set! a 123) a)
  
lala
123
asdf





(define (f x) (+ x 1))
(define (g x) (+ (f x) (f x)))
(g 0)

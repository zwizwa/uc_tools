(define (test_add a b) (add a b))
(define (test_if) (let* ((a (if 1 2 3))) a))
(define (test_assign a) (set! a 123) a)
(define (test_infix a b) (< a b))

(define (test_inner)
  (define (a) 123)
  (a 123))
    
  
1
2
3





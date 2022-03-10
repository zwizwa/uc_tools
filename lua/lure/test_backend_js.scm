;; JavaScript backend.

(+ 1 2)

(let ((a 1))
  (+ a 1))

(begin
  (define (f a) (g (+ a 1)))
  (define (g a) (f a))
  (f 0))

(if 1 2 3)



 
 
    

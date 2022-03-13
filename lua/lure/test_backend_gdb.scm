(begin
  (define (f x) x)
  (define (g x) (f x))
  (f 0))

(begin
  (define (f x) (g x))
  (define (g x)
    (if (> x 10) x (f (+ x 1))))
  (f 0))

  
  

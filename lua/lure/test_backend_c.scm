(begin
  (let ((a 1))
    (+ a a)))

(begin
  (define (x a b)
    (+ a b))
  (x 1 2))

(begin
  (define (f x) (g (+ x 1)))
  (define (g x) (f (* x x)))
  (f x))

(begin
  (define (f x)
    (define (a x)
      (if (> x 3)
          a
          (a (+ 1 x))))
    (g (+ x (a x))))
  (define (g x)
    (f (* x x)))
  (f x))




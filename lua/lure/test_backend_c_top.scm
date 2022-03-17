(begin
  (define (f x)
    (let ((x2 (* x x)))
      (+ 1 x2)))
  (trace (f 0)))


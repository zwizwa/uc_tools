(begin
  (define (f x)
    (let ((x2 (* x x)))
      (+ 1 x2)))
  (trace (f 0)))


(begin
  (define (g x)
    (let ((a (if (< x 10)
                 (* x x)
                 2)))
      (* a x)))
  (define (f x)
    (if x
        (g x)
        (let ((x2 (* x x)))
          (+ 1 x2))))
  (trace (f 0)))


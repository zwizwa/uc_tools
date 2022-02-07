;; Each expression is evaluated separately in test_scheme2.lua

123

(begin 1 2)

(+ 1 2)

(+ (+ 1 2) (+ 3 4))

(if (= 1 2) 123 456)

(begin (define (id x) x) (id 123))

(let ((a (+ 1 2))
      (b (+ 3 4)))
  (+ a b))

(let loop ((n 0))
  (if (> n 3) n (loop (+ n 1))))

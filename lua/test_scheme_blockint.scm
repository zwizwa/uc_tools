;; Each expression is evaluated separately in test_scheme2.lua

123

(begin 1 2)

(+ 1 2)

(+ (+ 1 2) (+ 3 4))

(assert
 (= 456
    (if (= 1 2) 123 456)))

(assert
 (= 123
    (begin (define (id x) x) (id 123))))

(assert
 (= 10
    (let ((a (+ 1 2))
          (b (+ 3 4)))
      (+ a b))))

(assert
 (= 4
    (let loop ((n 0))
      (if (> n 3) n (loop (+ n 1))))))
    
(assert
 (= 465
    (let loop ((n 2))
      (if (> n 10) n
          (+ (loop (+ n 1))
             (loop (* 2 n)))))))


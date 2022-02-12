;; Each expression is evaluated separately in test_scheme2.lua




;; First couple are for manual inspection.

;; Prim val
123

;; Prim fun
(+ 1 2)

;; Block sequencing
(begin 1 2)

;; Block sequencing + prim eval.
(+ (+ 1 2) (+ 3 4))

;; Closure call to inc, in tail and non tail position.
;; This is to test push/pop.  See 
(begin
  (define (inc x) (+ 1 x)) ;; closure
  (let ((tmp (inc 1)))     ;; non-tail call
    (inc tmp)))            ;; tail call



;; The rest use asserts.

(assert (= 3 (+ 1 (begin 1 2))))
(assert (= 3 (if #t (begin 1 2 3) (begin 4 5 6))))
(assert (= 6 (if #f (begin 1 2 3) (begin 4 5 6))))

(assert
 (= 465
    (let loop ((n 2))
      (if (> n 10) n
          (+ (loop (+ n 1))
             (loop (* 2 n)))))))

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
    

(assert 123 (call/cc (lambda (k) (k 123) 456)))
(assert 456 (call/cc (lambda (k) 456)))
(assert 234 (+ 111 (call/cc (lambda (k) (k 123) 456))))
(assert 567 (+ 111 (call/cc (lambda (k) 456))))
(assert 123 (call/cc (lambda (k) (+ 1 (+ 1 (+ 1 (k 123)))))))





           

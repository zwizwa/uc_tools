;; Each expression is compiled separately.

;; The compilation path is:
;; -> frontend -> flatten -> sm -> escape -> frontend -> flatten -> interp

;; Compiler output is converted back to Scheme and interpreted.  The
;; evaluation result is compared with the result of interpreting the
;; original Scheme code.

;; The trace function is used to abort execution after a fixed number
;; of calls.


;; Constructed to trigger bug in if continuation.
(begin
  (define (loop1 n)
    (trace)
    (loop1 (if #t (+ n 1) 0)))
  (loop1 0))


;; Constructed to trigger old scope issue.
;; FIXME: Currently fails
(begin
  (define (loop1 n)
    (let* ((a 1)
           (add1 (lambda (x) (+ a x))))
      
      (if (> n 3) 3
          (loop1 (add1 n)))))
              
  (define (loop2 n)
    (if n
        (loop1 (+ n 1))
        (loop1 (+ n 2))))
  (loop2 0))



123

;; Non-recursive function inline path.
(let* ((f (lambda (x) (+ x 1)))
       (rv (f 1)))
  rv)




;; Infinite single rec
(let loop ((n 0))
  (trace n)
  (if (> n 3)
      (loop 0)
      (loop (+ n 1))))



;; Simplest infinite loop
(letrec
    ((x (lambda ()
          (trace)
          (x))))
  (x))


;; Finite mutual rec loop
(begin
  (define (f1 n)
    (if (> n 3)
        n
        (f2 (+ n 1))))
  (define (f2 n)
    (f1 (+ n 1)))
  (let ((rv (f1 0)))
    (assert (= 5 (+ rv 1)))))

;; Test if contination
(if #t 1 2)
(if #f 1 (if #f 2 3))
(let ((a (if #t 1 2)))
  a)

(assert (= 1 (if #t 1 2)))
(assert (= 3 (if #f 1 (if #f 2 3))))
(assert (= 1 (let ((a (if #t 1 2))) a)))


;; The 'blockval' problem: convert binding to void binding + set! cont.
(let ((a
       (if 1
           (if (let ((b 2))
                 (+ b b))
               3
               4)
           (if 5
               6
               7))))
  (+ a a))


;; Infinite mutual rec
(begin
  (define (f1 n)
    (trace 'f1 n)
    (if (> n 3)
        (f1 0)
        (f2 (+ n 1))))
  (define (f2 n)
    (trace 'f2 n)
    (f1 (+ n 1)))
  (f1 0))



;; Nested loops
;(begin
;  (let loop1 ((i 0))
;    (if (> i 3) i
;        (let loop2 ((j 0))
;          (if (> (+ i j) 3) j
;              (loop2 (+ j 1)))))))

;; Finite nested
(begin
  (let ((done 123))
    (let loopi ((i 0))
      (if (> i 3) done
          (begin
            (let loopj ((j 0))
              (if (> j i)
                  done
                  (loopj (+ j 1))))
            (loopi (+ i 1)))))))


;; Finite single
(let loop ((n 0))
  (if (> n 3)
      n
      (loop (+ n 1))))


;; Finite single, twice
(begin
  (define (loop n)
    (if (> n 3)
        n
        (loop (+ n 1))))
  (let* ((a (loop 0))
         (b (loop 0)))
    (+ a b)))


;; Constructed to trigger old scope issue.
(begin
  (define (loop1 n)
    (if (> n 3) 3 (loop1 (+ n 1))))
              
  (define (loop2 n)
    (if n
        (loop1 (+ n 1))
        (loop1 (+ n 2))))
  (loop2 0))





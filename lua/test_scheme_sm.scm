;; Each expression is compiled separately.

;; The compilation path is:
;; -> frontend -> flatten -> sm -> escape -> frontend -> flatten -> interp

;; Simplest infinite loop
(letrec
    ((x (lambda ()
          (trace 'tick)
          (x)))) (x))


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


;; Infinite single rec
(let loop ((n 0))
  (trace 'tick)
  (if (> n 3)
      (loop 0)
      (loop (+ n 1))))


;; The 'blockval' problem: convert binding to void binding + set! cont.
(let ((a (if 1
             (if (let ((b 2)) (+ b b)) 3 4)
             (if 5 6 7))))
  (+ a a))


;; Infinite mutual rec
(begin
  (define (f1 n)
    (trace 'tick)
    (if (> n 3)
        (f1 0)
        (f2 (+ n 1))))
  (define (f2 n)
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


;; Constructed to break the label lexical scope rules.
(begin
  (define (loop1 n)
    (if (> n 3) 3 (loop1 (+ n 1))))
              
  (define (loop2 n)
    (if n
        (loop1 (+ n 1))
        (loop1 (+ n 2))))
  (loop2 0))

;; Similar, but scope issue prevents.

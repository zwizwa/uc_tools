;; Each expression is compiled separately.

;; The 'blockval' problem: convert binding to void binding + set! cont.
(let ((a (if 1 2 3)))
  (+ a a))

;; Infinite single rec
(let loop ((n 0))
  (if (> n 3)
      (loop 0)
      (loop (+ n 1))))

;; Infinite mutual rec
(begin
  (define (f1 n)
    (if (> n 3)
        (f1 0)
        (f2 (+ n 1))))
  (define (f2 n)
    (f1 (+ n 1)))
  (f1 0))

;; Finite mutual rec loop
(begin
  (define (f1 n)
    (if (> n 3)
        n
        (f2 (+ n 1))))
  (define (f2 n)
    (f1 (+ n 1)))
  (let ((rv (f1 0)))
    (+ rv 1)))


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
  (loop 0)
  (loop 0))


;; 123

;; (begin 1 2)

;; (+ 1 2)

;; (+ (+ 1 2) (+ 3 4))

;; (if (= 1 2) 123 456)

;; (begin (define (id x) x) (id 123))

;; (let ((a (+ 1 2))
;;       (b (+ 3 4)))
;;   (+ a b))

;; (let loop ((n 0))
;;   (if (> n 3) n (loop (+ n 1))))

;; ;; Should fail compilation due to inline loop.
;; (let loop ((n 2))
;;   (if (> n 10) n
;;       (+ (loop (+ n 1))
;;          (loop (* 2 n)))))


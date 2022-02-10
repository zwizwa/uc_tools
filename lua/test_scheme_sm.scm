;; Each expression is compiled separately.

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


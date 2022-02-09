;; Each expression is compiled separately.

;; Note that the state machine compiler only compiles infinite loops,
;; so none of these expressions will terminate.

(let loop ((n 0))
  (if (> n 3)
      (loop 0)
      (loop (+ n 1))))


(begin
  (define (f1 n)
    (if (> n 3)
        (f1 0)
        (f2 (+ n 1))))
  (define (f2 n)
    (f1 (+ n 1)))
  (f1 0))



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


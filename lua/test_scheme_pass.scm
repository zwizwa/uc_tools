;; (define (id x) x)
;; (lambda (x) x)
(define (fun0 x) x)

(define (fun1 q)
  (let* ((a 1)
         (b (if q 123 456))
         (c 3)
         (f (lambda (x y) (add x y)))
         )
    (if a
        a
        (add a (f b c)))))


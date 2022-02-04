;; (define (id x) x)
;; (lambda (x) x)

(lambda (q)
  (let* ((a 1)
         (b (if q 123 456))
         (c 3)
         (f (lambda (x y) (add x u)))
         )
    (if a
        a
        (add a (f b c)))))

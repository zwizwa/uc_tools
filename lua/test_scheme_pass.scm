;; (define (id x) x)
;; (lambda (x) x)

(lambda (q)
  (let* ((a 1)
         (b (if q 123 456))
         (c 3))
    (if a
        a
        (add a (add b c)))))

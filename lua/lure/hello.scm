;; Scheme
;; Same form as .sm
(module (test)
  (define (f a)
    (let* ((b (add a a)))
      (add a b)))
  (f 3))
;(module (test)
;  (define (main)
;    (add 1 2)))

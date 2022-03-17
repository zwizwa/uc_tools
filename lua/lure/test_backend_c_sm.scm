;; These are all in state machine mode, where the output is a single C
;; function, and Scheme functions map to goto labels.  Later, add top
;; level mode where the outer labels form maps to C functions.

(begin
  (let ((a 1))
    (+ a a)))

(begin
  (define (x a b)
    (+ a b))
  (x 1 2))

(begin
  (define (f x)
    (trace x)
    (g (+ x 1)))
  (define (g x)
    (f (* x x)))
  ;; (f x)  ;; Gives obscure error due to missing ephemeral variable.
  (f 0)
  )

(begin
  (define (f x)
    (define (a x)
      (if (> x 3)
          ;; a ;; FIXME: Old type error referenced function here,
          ;; which didn't cause an error, just an undefined reference.
          (begin
            (trace x)
            (g x))
          (a (+ 1 x))))
    (a (+ x x)))
  (define (g x)
    (f (* x x)))
  (f 0))



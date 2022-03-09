;; Erlang supports only a subset of Scheme.  Two important restrictions:
;; 1. No assignment. The scheme_frontend is configured to use primitive letrec
;; 2. Mutual recursion only works at the module level.

;; Each form here is a separate Erlang module, represented as a top
;; level letrec form.  The body of such a letrec is treated as a
;; separate function which we expose as 'test'.

(begin
  (define (main)
    (let ((a (+ 1 2)))
      (if a 1 2)))
  (void))

(begin
  (define (f x) (g (+ x 1)))
  (define (g x) (f x))
  (f 0))

(begin
  (define (f)
    (let ((a 1)
          (b 2))
      (+ a b)))
  (void))

(begin
  (define (f x)
    (let ((a 1)
          (b 2))
      (f (+ x (+ a b)))))
  (f 0))

(begin
  (define (f x)
    (let ((a 1)
          (b 2))
      (if x x
          (f (+ x (+ a b))))))
  (f 0))

(begin
  (define (f x)
    (let ((a (lambda (x) (+ x 1)))
          (b (lambda (x) (+ x 2))))
      (f (a (b x)))))
  (f 0))

(begin
  (define (f x)
    (let ((a (lambda (x)
               (let ((c 1))
                 (+ x c))))
          (b (lambda (x)
               (let ((c 2))
                 (+ x c)))))
      (f (a (b x)))))
  (f 0))




 

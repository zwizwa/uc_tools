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

;; FIXME: Mutual recursion is possible, but only at the top level of a
;; module.  This is fairly limiting...
(begin
  (define (f x) (g (+ x 1)))
  (define (g x) (f x))
  (f 0))


 

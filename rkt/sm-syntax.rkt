#lang scheme/base
;; Modeled after rai/sm-syntax

;; The compiler is indirect: all functions are converted to higher
;; order syntax, so we can use abstract interpretation to
;; evaluate/compile it to multiple targets, i.e. C for inclusion in
;; embedded controller code, and Racket (or Haskell?) for analysis.

(require
 racket/stxparam
 (for-syntax
  racket/base))

         
(provide (all-defined-out))



(define-syntax-parameter sm-semantics (lambda (stx) #'#f))


(define sm-function-printer
  (make-parameter
   (lambda (fun port mode)
     (when mode (write-string "#<" port))
     (write-string "sm-function " port)
     (write-string (format "~a" (sm-function-args fun)) port)
     (when mode (write-string ">" port)))))
(define-struct sm-function (proc args)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc (lambda args (apply (sm-function-printer) args)))])

(define-syntax-rule (sm-app fn a ...)
  ((sm-function-proc fn)
   (sm-semantics)
   a ...))

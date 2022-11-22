return require("lure") -- ; -*- scheme -*-
["slc"](
[[

(define-syntax test-mac
  (lambda (stx)
    (return `(begin . ,(cdr stx)))))
  
;(define-macro (test-mac . args)
;  `(begin . ,args))
  
(define (edsl semantics)
  
(define (ref sym) (table-ref semantics sym))
(let ((+    (ref 'add))
      (-    (ref 'sub))
      (rec1 (ref 'rec1)))

(define (integrate i)
  (rec1
   (lambda (s)
     (values (+ s i) s))))

(table
 `((integrate . ,integrate)))
))

;; Test code.
(define (run)
  (define rec1 'FIXME)
  (let ((semantics
         (table
          `((add  . ,+)
            (sub  . ,-)
            (rec1 . ,rec1)))))
    (log-se-n
     (table-ref (edsl semantics) 'integrate))
    ))

]])

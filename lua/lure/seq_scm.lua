return require("lure") -- ; -*- scheme -*-
["slc"](
[[
(define (edsl c)
(let ((+    (table-ref c 'add))
      (-    (table-ref c 'sub))
      (rec1 (table-ref c 'rec1)))

;; FIXME: Create a macro facility.

(define (integrate i)
  (rec1
   (lambda (s)
     (values (+ s i) s))))

(table
 (list
  (list 'integrate integrate)))
))

;; Test code.
;; FIXME: quasiquote & unquote
(define (run)
  (define rec1 'FIXME)
  (let ((c
         (table
          (list
           (list 'add +)
           (list 'sub -)
           (list 'rec1 rec1)
           ))))
    (log-se-n
     (table-ref (edsl c) 'integrate))
    ))

]])

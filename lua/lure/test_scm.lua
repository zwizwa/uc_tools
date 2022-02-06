return require("lure") -- ; -*- scheme -*-
["slc2"](
[[
;; The 'lure' module is the index into the language dialects
;; implemented using Lure.  This file is implemented in slc2, the
;; second version of the Lua to Scheme compiler.  The function slc2
;; takes a string, compiles it to Lua code and evaluates it.  The
;; result is a map containing all top level Scheme definitions in this
;; file.  Note that for release it is possible to pre-compile these
;; files into their Lua representation to avoid the cost of runing the
;; Scheme compiler at load time.

;; The comment at the first line is for Emacs to perform correct
;; syntax higlighting for this file, i.e. selecting scheme-mode
;; instead of Lua.

(define (f x) (+ x x))

(define (match-add e) (match-qq e ((add ,a ,b) (+ a b))))
(define (test_match)  (assert (= 3 (match-add '(add 1 2)))))

(define (test_loop)
  (assert
   (= 4
      (let loop ((n 0))
        (if (> n 3)
            (begin
              ;;(log-se-n n 'NAMDEDLETDONE:)
              n)
            (begin
              ;;(log-se-n n 'NAMEDLET:)
              (loop (+ n 1))))))))

;; FIXME: letrec doesn't work properly if the initial call into the
;; network is not a tail call.
(define (example_letrec)
  (define (check n) (if (> n 4) n (inc n)))
  (define (inc n)   (begin (log-se-n n 'LETREC:) (check (+ n 1))))
  (check 0)  ;; needs to be tail call
  )
(define (test_letrec)
  (assert (= 5 (example_letrec))))


(define (run)
  (assert (= 2 (f 1)))
  ;(assert (eqv? (list 1 2 3) '(1 2 3)))) ;; HANGS
  (test_match)
  (test_letrec)
  (test_loop)
  )
  

;; Additional
]],{verbose=true})


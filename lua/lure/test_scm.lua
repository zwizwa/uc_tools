return require("lure") -- ; -*- scheme -*-
["slc"](
[[
;; The 'lure' module is the index into the language dialects
;; implemented using Lure.  This file is implemented in slc, the
;; second version of the Lua to Scheme compiler.  The function slc
;; takes a string, compiles it to Lua code and evaluates it.  The
;; result is a map containing all top level Scheme definitions in this
;; file.  Note that for release it is possible to pre-compile these
;; files into their Lua representation to avoid the cost of runing the
;; Scheme compiler at load time.

;; The comment at the first line is for Emacs to perform correct
;; syntax higlighting for this file, i.e. selecting scheme-mode
;; instead of Lua.

;; Let's start out with how Lure SLC is different from Scheme.

;; 1. No continuation support
;; 2. No implicit constant-space tail recursion
;;
;; These are consequences of compilation of Scheme functions as Lua
;; functions.
;;
;; To mitigate lack of constant-space tail recursion, there is support
;; for trampoline variants of Scheme recursive forms:
;;
;;  letrec    -> letrec@
;;  begin     -> begin@ (expands to letrec-trampoline)
;;  let       -> let@ (named let expands to letrec-trampoline)
;;
;; Note that using those forms, only tail recursive calls are allowed.
;; The trampoline mechanism does not work for non-tail calls.  There
;; is no way to test for this!
;;
;; So... support is here in case it is needed to not have to rewrite
;; complex functions that do obey the tail call constraint, but it is
;; probably best to avoid these and instead use a recursive combinator.

(define (test_named_let)
  (assert
   (= 4
      (let@ next ((n 0))
        (if (> n 3)
            (begin
              ;;(log-se-n n 'NAMDEDLETDONE:)
              n)
            (begin
              ;;(log-se-n n 'NAMEDLET:)
              (next (+ n 1))))))))

(define (test_letrec)
  (begin@
   (define (check n)
     (if (> n 4) n (inc n)))
   (define (inc n)
     ;; (log-se-n n 'LETREC:)
     (check (+ n 1)))
   (assert (= 5 (check 0)))))


  
  

(define (f x) (+ x x))

(define (match-add e) (match-qq e ((add ,a ,b) (+ a b))))
(define (test_match)  (assert (= 3 (match-add '(add 1 2)))))




(define (run)
  (test_letrec)
  (test_named_let)
  
  (assert (= 2 (f 1)))
  ;(assert (eqv? (list 1 2 3) '(1 2 3)))) ;; HANGS
  (test_match)
  )
  

;; Additional
]],{verbose=false})


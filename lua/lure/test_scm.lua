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

;; Recursive/loop combinators
;;
;; foldr is recursive, foldl is implemented using a Lua while loop
(define (test_foldl) (assert (= 6 (foldl + 0 '(1 2 3)))))
(define (test_foldr) (assert (= 6 (foldr + 0 '(1 2 3)))))
(define (test_map)   (assert (equal? '(2 3 4) (map (lambda (x) (+ x 1)) '(1 2 3)))))
  
(define (match-add e) (match-qq e ((add ,a ,b) (+ a b))))
(define (test_match)  (assert (= 3 (match-add '(add 1 2)))))


;; Dynamic binding
(define par (make-parameter 1))
(define (test_par_is n)
  (assert (= n (par))))
(define (test_par)
  (assert (= 1 (par)))
  (parameterize ((par 2))
    (assert (= 2 (par)))
    (test_par_is 2)
    )
     
  (test_par_is 1)
  (assert (= 1 (par)))
  )

(define (run)
  (test_letrec)
  (test_named_let)
  (test_foldl)
  (test_foldr)
  (test_map)
  (test_par)
  
  (test_match)
  )
  

;; Additional config.
]],{verbose=false})




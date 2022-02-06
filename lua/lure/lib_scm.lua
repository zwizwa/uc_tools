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
(define (test_match) (log-se (match-add '(add 1 2))))
(define x '(1 2 3))

(define (test_loop)
  (let loop ((n 0))
    (if (> n 3) 'done
        (begin
          (log-se-n n 'LOOP)
          (loop (+ n 1))))))
        


;; Additional
]],{verbose=true})


#lang racket/base
(provide indented w write-c)
;; Naive C code generator.
;; Derived from lua/lib/smc.lua

;; The Lua form used Erlang style IOLists to represent output text.
;; This makes it easy to separate writing to the output and combining
;; snippets of C code.

(define indent (make-parameter 1))
(define write-c (make-parameter write-string)) ;; write to current C output buffer
(define (write-iolist iolist)
  (for ((el iolist))
    (if (string? el)
        ((write-c) el)
        (write-iolist el))))
(define (w . args)
  (write-iolist args))
  
(define (tab) (for/list ((_ (in-range (indent)))) "  "))

(define-syntax-rule (indented body)
  (parameterize ((indent (+ (indent) 1))) body))

;; FIXME: In the Lua code, there is some ad-hoc abstract
;; representation of C syntax elements, but it is not separated out
;; very well.  Maybe use the Racket coding process as a way to
;; serparate the original compiler?  I do want to keep it running.

;; Can't make it too general at this point.  Code gen depends on the
;; representation variables, which can either be C local variables, or
;; an element in a "stack array" in a state structure.
(define-struct cell (id bind index const multipath) #:transparent)
(define-struct var  (name cell))

;; Also, we assume the environment is accessible, mapping variable
;; names to var objects.
(define env (make-parameter '()))
  
;; Most expressions are compiled in a context that captures their
;; value.  In most (all?) cases this is a variable in a let
;; expression.  The code emitter needs to generate the variable
;; definition if this parameter is true.
(define current-var (make-parameter #f))

(define state-name (make-parameter "s"))
(define word-type (make-parameter "T"))

;; Clumsy: create a routine that does all 4 cases:
;; - define local with and without assignent
;; - assignment to struct var
;; - assignment to c var
(define (cvar-and-ctype v)
  (let* ((comment (list "/*" (var-name v) "*/"))
         (cell (var-cell cell))
         (index (cell-index cell))
         (id (cell-id cell)))
    (if index
        (values
         (list (state-name) "->e[" index "]" comment)
         "")
        (values
         (list "r" id)
         (list (word-type) " ")))))

(define (var-def var)
  (let-values (((cvar ctype) (cvar-and-ctype var)))
    (if (cell-multipath (var-cell var))
        ;; Assume variable definition has already been written out
        ;; without value.
        (list cvar " = ")
        (list ctype cvar " = "))))
    
(define (w-binding c-expr)
  (w (tab))
  (when (current-var)
    (w (var-def (current-var)))
    ;; (mark-bound (current-var))
    )
  (w c-expr ";\n"))
      

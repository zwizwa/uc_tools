;; Copy of test_rvm.scm, to be hollowed out by gradually replacing
;; components with Lua implementations while keeping it running, to
;; eventually end up with a working rvm.lua interpreter.


;; This file is modified from:
;; https://github.com/udem-dlteam/ribbit
;; https://github.com/udem-dlteam/ribbit/blob/main/src/host/scm/rvm.scm
;; BSD 3-Clause License

;; The idea is to modify this such that it can be compiled by uc_tools
;; slc.lua and possibly further minified.

;; Changes to make it work for now.  To revisit later.
;; - uc_tools/lua/lib/se.lua doesn't support strings: replaced with quoted atoms
;; - removed cond-expand debug and gambit clauses
;; - changed '#(...) to (vector ...)
;; - wrapped infix primitives

(define pair-type      0)
(define procedure-type 1)
(define symbol-type    2)
(define string-type    3)
(define vector-type    4)
(define singleton-type 5)

(define (_rib? x) (vector? x))
;; (define (_rib x y z) (vector x y z))
(define (_field0 x) (vector-ref x 0))
(define (_field1 x) (vector-ref x 1))
(define (_field2 x) (vector-ref x 2))
(define (_field0-set! x y) (vector-set! x 0 y))
(define (_field1-set! x y) (vector-set! x 1 y))
(define (_field2-set! x y) (vector-set! x 2 y))

(define (instance? type)
  (lambda (x) (and (_rib? x) (eqv? (_field2 x) type))))

(define _pair? (instance? pair-type))
(define (_cons car cdr) (_rib car cdr pair-type))
(define (_car pair) (_field0 pair))
(define (_cdr pair) (_field1 pair))
(define (_set-car! pair x) (_field0-set! pair x))

(define (_list->string lst) (_rib lst (_length lst) string-type))

(define (_string->uninterned-symbol str) (_rib _false str symbol-type))

;(define _false (_rib 0 0 singleton-type))
;(define _true  (_rib 0 0 singleton-type))
;(define _nil   (_rib 0 0 singleton-type))

(define (_list-tail lst i)
  (if (< 0 i)
      (_list-tail (_cdr lst) (- i 1))
      lst))

(define (_length lst)
  (if (_pair? lst)
      (+ 1 (_length (_cdr lst)))
      0))

;; (define pos 0)

;(define (get-byte)
;  (let ((x (char->integer (string-ref input pos))))
;    (set! pos (+ pos 1))
;    (log-se-n (list pos x))
;    x))

;; ; FIXME: The prim 18 switches from git-byte to read-char based on pos,
;; ; so keep track of pos.
;; (define (get-byte)
;;   (let ((x (get-input-byte)))
;;     ;; (set! pos (+ pos 1))
;;     ;; (log-se-n (list pos x))
;;     x))


;; (define (decode)

;; ; (define eb/2 46) ;; half of encoding base (92)


;; ;  (define (get-int n)
;; ;    (let ((x (get-code))
;; ;          (y (* n eb/2)))
;; ;      (if (< x eb/2)
;; ;          (+ y x)
;; ;          (get-int (+ y (- x eb/2))))))

;;   ;; (define (build-symtbl)

;;   ;;   (define (add-symbol chars symtbl)
;;   ;;     ;; (log-se-n chars)
;;   ;;     (_cons (_string->uninterned-symbol (_list->string chars))
;;   ;;            symtbl))

;;   ;;   (let loop1 ((n (get-int 0)) (symtbl _nil))
;;   ;;     (if (< 0 n)
;;   ;;         (loop1 (- n 1) (add-symbol _nil symtbl))
;;   ;;         (let loop2 ((symtbl symtbl))
;;   ;;           (let loop3 ((chars _nil))
;;   ;;             (let ((x (get-byte)))
;;   ;;               (if (= x 44) ;; #\, separates symbols
;;   ;;                   (loop2 (add-symbol chars symtbl))
;;   ;;                   (if (= x 59) ;; #\; terminates symbol list
;;   ;;                       (add-symbol chars symtbl)
;;   ;;                       (loop3 (_cons x chars))))))))))

;;   (let ((symtbl (build-symtbl-lua)))

;; ;;     (define (decode-loop stack)

;; ;; ;      (define (sym n)
;; ;; ;        (_car (_list-tail symtbl n)))

;; ;;       (define (add-instruction op opnd stack)
;; ;; ;;        (pp (list (vector-ref '#(jump/call set get const if) op) opnd))
;; ;;         (_set-car! stack (_rib op opnd (_car stack)))
;; ;;         (decode-loop stack))

;; ;;       (let ((x (get-code)))
;; ;;         (log-se-n (list 'get-code x))
;; ;;         (let loop ((op 0) (n x))
;; ;;           (let ((d (vector-ref (vector 20 30 0 10 11 4) op)))
;; ;;             (if (< (+ 2 d) n)
;; ;;                 (loop (+ op 1) (- n (+ d 3)))
;; ;;                 (if (< 90 x)
;; ;;                     (add-instruction 4 ;; if
;; ;;                                      (_car stack)
;; ;;                                      (_cdr stack))
;; ;;                     (let ((stack (if (= op 0) (_cons 0 stack) stack))
;; ;;                           (opnd (if (< n d)
;; ;;                                     (if (< op 3)
;; ;;                                         (sym n)
;; ;;                                         n)
;; ;;                                     (if (= n d)
;; ;;                                         (get-int 0)
;; ;;                                         (sym (get-int (- (- n d) 1)))))))
;; ;;                       (if (< 4 op)
;; ;;                           (let ((proc (_rib
;; ;;                                        (_rib opnd 0 (_car stack))
;; ;;                                        _nil
;; ;;                                        procedure-type))
;; ;;                                 (stack (_cdr stack)))
;; ;;                             (if (_rib? stack)
;; ;;                                 (add-instruction 3 ;; const-proc
;; ;;                                                  proc
;; ;;                                                  stack)
;; ;;                                 proc))
;; ;;                           (add-instruction (if (< 0 op) (- op 1) 0)
;; ;;                                            opnd
;; ;;                                            stack)))))))))

;;     (let ((main-proc
;;            ;; (decode-loop 0)
;;            (decode-lua)
;;            ))

;;       ;; set predefined globals (always 4 first in the symbol table)

;;       (define (set-global val)
;;         (_field0-set! (_car symtbl) val)
;;         (set! symtbl (_cdr symtbl)))
;; ;;(pp (list 'pos= pos))
;; ;;(pp (list 'symtbl= (convert symtbl)))

;;       (set-global (_rib 0 symtbl procedure-type)) ;; rib  = primitive 0
;;       (set-global _false) ;; false  = #f
;;       (set-global _true)  ;; true   = #t
;;       (set-global _nil)   ;; nil    = ()

;;       main-proc)))


(define (run1 pc stack)
  (define count 0)

  (let run ((pc pc) (stack stack))

  (define (get-cont stack)
    (let loop ((stack stack))
      (if (_rib? (_field2 stack)) stack (loop (_cdr stack)))))

  (define (get-var opnd)
    (_field0 (if (_rib? opnd) opnd (_list-tail stack opnd))))

  (define (set-var opnd val)
    (_field0-set! (if (_rib? opnd) opnd (_list-tail stack opnd)) val))

  ;; (log-se-n (_field0 pc))

  (let ((instr (_field0 pc))
        (opnd (_field1 pc))
        (next (_field2 pc)))
    (set! count (+ count 1))
    ;; (log-se-n (list count instr))

    (case instr

      ((0) ;; jump/call
       (trace-instruction (if (eqv? 0 next) 'jump 'call) opnd stack)
       (let* ((proc (get-var opnd))
              (code (_field0 proc)))
         (if (_rib? code)

             ;; calling a lambda
             (let ((new-cont (_rib 0 proc 0)))
               (let loop ((nargs (_field0 code))
                          (new-stack new-cont)
                          (stack stack))
                 (if (< 0 nargs)
                     (loop (- nargs 1)
                           (_cons (_car stack) new-stack)
                           (_cdr stack))
                     (begin
                       (if (_rib? next) ;; non-tail call?
                           (begin
                             (_field0-set! new-cont stack)
                             (_field2-set! new-cont next))
                           (let ((k (get-cont stack)))
                             (_field0-set! new-cont (_field0 k))
                             (_field2-set! new-cont (_field2 k))))
                       (run (_field2 code)
                            new-stack)))))

             ;; calling a primitive
             (let ((stack ((vector-ref primitives code) stack)))
               ;; (log-se-n (list 'prim code))
               (run (if (_rib? next) ;; non-tail call?
                        next
                        (let ((cont (get-cont stack)))
                          (_field1-set! stack (_field0 cont))
                          (_field2 cont)))
                    stack)))))

      ((1) ;; set
       (trace-instruction 'set opnd stack)
       (set-var opnd (_car stack))
       (run next
            (_cdr stack)))

      ((2) ;; get
       (trace-instruction 'get opnd stack)
       (run next
            (_cons (get-var opnd) stack)))

      ((3) ;; const
       (trace-instruction 'const opnd stack)
       (run next
            (_cons opnd stack)))

      ((4) ;; if
       (trace-instruction 'if #f stack)
       (run (if (eqv? (_car stack) _false) next opnd)
            (_cdr stack))))))
)

(define (prim0 f)
  (lambda (stack)
    (_cons (f) stack)))

(define (prim1 f)
  (lambda (stack)
    (let* ((x (_car stack)) (stack (_cdr stack)))
      (_cons (f x) stack))))

(define (prim2 f)
  (lambda (stack)
    (let* ((y (_car stack)) (stack (_cdr stack))
           (x (_car stack)) (stack (_cdr stack)))
      (_cons (f x y) stack))))

(define (prim3 f)
  (lambda (stack)
    (let* ((z (_car stack)) (stack (_cdr stack))
           (y (_car stack)) (stack (_cdr stack))
           (x (_car stack)) (stack (_cdr stack)))
      (_cons (f x y z) stack))))

(define (boolean x)
  (if x _true _false))

(define primitives
  (vector (prim3 _rib)             ;; 0
          (prim1 (lambda (x) x))   ;; 1
          _cdr                     ;; 2
          (prim2 (lambda (y x) x)) ;; 3

          (lambda (stack) ;; 4
            (let* ((x (_car stack)) (stack (_cdr stack)))
              (_cons (_rib (_field0 x) stack procedure-type) stack)))

          (prim1 (lambda (x) (boolean (_rib? x)))) ;; 5
          (prim1 _field0) ;; 6
          (prim1 _field1) ;; 7
          (prim1 _field2) ;; 8
          (prim2 (lambda (x y) (_field0-set! x y) y)) ;; 9
          (prim2 (lambda (x y) (_field1-set! x y) y)) ;; 10
          (prim2 (lambda (x y) (_field2-set! x y) y)) ;; 11
          (prim2 (lambda (x y) (boolean (eqv? x y)))) ;; 12
          (prim2 (lambda (x y) (boolean (< x y)))) ;; 13
          (prim2 (lambda (x y) (+ x y))) ;; 14
          (prim2 (lambda (x y) (- x y))) ;; 15
          (prim2 (lambda (x y) (* x y))) ;; 16
          (prim2 quotient) ;; 17

          (prim0 get-byte-or-read-char) ;; 18

          (prim1 (lambda (x) ;; 19
                   (write-char (integer->char x))
                   x))))

;; This doesn't work. API is different.
;; (vector-set! primitives 0 (vector-ref primitives-lua 0))


(define (run)
  (let ((x (decode-lua)))
    ;; (desc x)
    (run1 (_field2 (_field0 x)) ;; instruction stream of main procedure
          (_rib 0 0 (_rib 5 0 0)))) ;; primordial continuation = halt
  )

;; (run)
(run-lua)

;; The idea here is to create a transparant tethered structure.  The
;; end goal is some C code that does its thing on the microcontroller,
;; detached from the scaffolding.  During develoment there is
;; scaffolding that allows:
;;
;; 1. Exploration of the naked target, without it running ANY support code.
;;
;; 2. Gradual transition from host running code talking to remote
;;    registers to host calling some routines on target, to target
;;    being independent.
;;

;; Let's start with a simple example.  Two functions, g calling f.
;; Set this up in 3 ways: both on host, f on target, g on host, and f
;; and g both on target.  The 'glue' that composes this is a compiler
;; directive that indicates what goes where.

;; (target f)
;; or
;; (target f g)


(define (test_loop) ;; wrap to skip top level
  (let ((rv ;; bind so rv prop can be checked
         (let loop ((i 0))
           (if (> i 3)
               123 ;; recognizable return value
               (begin
                 (print i)
                 (loop (+ i 1)))))))
    (+ rv 345) ;; use so rv prop can be checked
    ))


(define (f x) (+ x 10))
(define (g x) (+ (f x) (f (+ 100 x))))

;; (g 1)

(test_loop)





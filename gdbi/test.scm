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

(define (test_2loop) ;; wrap to skip top level
  (let ((rv ;; bind so rv prop can be checked
         (let loop1 ((i 0) (ii 2))
           (if (> i 3)
               123 ;; recognizable return value
               (let loop2 ((j 0))
                 (if (> j 3)
                     (loop1 (+ i 1) (* ii ii))
                     (begin
                       (print (vector i j ii))
                       (loop2 (+ j 1)))))))))
    (+ rv 345) ;; use so rv prop can be checked
    ))

(define (trice f)
  (vector (f 1) (f 2) (f 3)))
(define (test_lambda)
  (let* ((captured 123))
    (trice (lambda (x) (+ captured x)))))

;; Test for moving from target to host.  The begin-target form will
;; compile forms to C and create gdb 'call' wrappers for them.
(begin-target
 (define (f x) (+ x 10))
 (define (g x) (+ (f x) (f (+ 100 x)))))

;; TODO:
;;
;; - Toplevel GDB and C functions should not keep names if they are
;;   correct C identifiers.



;; (g 1)
(test_2loop)

;; (test_lambda)
;; (test_lambda)





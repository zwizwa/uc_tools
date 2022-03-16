;; Experimentail.  Main drawback is lack of re-entrancy due to lack of
;; local variables.  See backend_gdb.scm for more comments.

(begin
  (define (f x) x)
  (define (g x) (f x))
  (f 0))

(begin
  (define (f x) (g x))
  (define (g x)
    (if (> x 10) x (f (+ x 1))))
  (f 0))

  
;; Named let (letrec with one recursive function) is converted to a
;; while loop.

(begin
  (define (test_loop) ;; wrap to skip top level
    (let ((rv ;; bind so rv prop can be checked
           (let loop ((i 0))
             (if (> i 3)
                 123 ;; recognizable return value
                 (loop (+ i 1))))))
      (+ rv 345) ;; use so rv prop can be checked
      )))


;; Double loop with print & vector.
(begin
  (define (test_loop) ;; wrap to skip top level
    (let ((rv ;; bind so rv prop can be checked
           (let loop1 ((i 0))
             (if (> i 3)
                 123 ;; recognizable return value
                 (let loop2 ((j 0))
                   (if (> j 3)
                       (begin
                         (print (vector i j))
                         (loop1 (+ i 1)))
                       (loop2 (+ j 1))))))))
      (+ rv 345) ;; use so rv prop can be checked
      )))

;; Anonymous functions.
(begin
  (define (trice f)
    (vector (f 1) (f 2) (f 3)))
  (define (test_lambda)
    (let* ((captured 123))
      (print (trice (lambda (x) (+ captured x))))))
  (test_lambda))


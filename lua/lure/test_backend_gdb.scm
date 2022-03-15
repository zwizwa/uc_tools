(begin
  (define (f x) x)
  (define (g x) (f x))
  (f 0))

(begin
  (define (f x) (g x))
  (define (g x)
    (if (> x 10) x (f (+ x 1))))
  (f 0))

  
  

;; ;; This generates code that is supposed to be captured in a while
;; ;; loop.  Note that a while loop is imperative, so the return value is
;; ;; void.  I'd like two things: a macro that maps while to a named let,
;; ;; and a matcher that maps it back to a while loop.

;; (let ((context 0))
;;   (letrec
;;       ((loop
;;         (lambda (i)
;;           (if (> i 3)
;;               (void) ;; Stop condition
;;               (begin
;;                 (set! context i) ;; Some side-effect
;;                 (loop (+ i 1)))))))
;;     (loop 0)))

;; ;; Should it be that, or should I make it effectful?

;; (let ((context 0)
;;       (i 0))
;;   (letrec
;;       ((loop
;;         (lambda ()
;;           (if (> i 3)
;;               (void) ;; Stop condition
;;               (begin
;;                 (set! context i) ;; Some side-effect
;;                 (set! i (+ i 1))
;;                 (loop))))))
;;     (loop)))


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

      

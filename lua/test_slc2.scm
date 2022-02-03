(block (
  (fun1
    (lambda (x)
      (block (
        (_ (if x
             (block ((_ (set! rv 1))))
             (block ((_ (set! rv 2))))))))))
  (fun2
    (lambda ()
      (block (
        (f (lambda (x) (block ((_ (return x))))))
        (a 123)
        (_ (set! a 456))))))
))


#lang racket/base
;; Wrapper for smc-syntax.rkt implementing a Racket #lang
;; Same style as rai/stream.rkt
(require
 "sm-syntax.rkt"
 )
(provide
 ;; Re-use racket basic syntax
 #%module-begin
 #%top
 #%top-interaction
 #%datum
 ;; Other racket bindings
 provide all-defined-out define

 ;; Modified syntax
 (rename-out
  (sm-app #%app))
 )


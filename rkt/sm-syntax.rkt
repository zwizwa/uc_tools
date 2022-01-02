#lang scheme/base
(provide (all-defined-out))
(define-syntax-rule (sm-app fn a ...)
  (fn a ...))

#lang racket

(require racket/base)

(provide
  (struct-out gd-prop)
  (struct-out gd-method)
  (struct-out gd-class)
  gd-type-variant
  gd-type-float)

; These must be parameterized
(define gd-type-variant 1)
(define gd-type-float 3)

(struct gd-prop (name type) #:reflection-name 'gd-prop)
(struct gd-method (name return-type arguments) #:reflection-name 'gd-method)
(struct gd-class (name methods properties) #:reflection-name 'gd-class)

#lang racket

(require racket/base)
(require (prefix-in gdp: 'gd-primitive))


(define-syntax (defprovide stx)
  (let* ([stx-list (syntax->list stx)]
         [sym (cadr stx-list)]
         [expr (caddr stx-list)])
  (datum->syntax stx `(begin (define ,sym ,expr) (provide ,sym)))))


(define-syntax-rule (values->list EXPR)
  (call-with-values (λ () EXPR) list))


(provide (struct-out gd-prop))
(struct gd-prop (name type) #:transparent #:reflection-name 'gd-prop)


(provide (struct-out gd-method))
(struct gd-method (name return-type arguments) #:transparent #:reflection-name 'gd-method)


(provide (struct-out gd-class))
(struct gd-class (name methods properties) #:transparent #:reflection-name 'gd-class)


(defprovide print
  (λ ... (displayln ...) (flush-output)))


(defprovide push-error
  (λ ... (gdp:push-error (apply string-append ...))))


(defprovide push-exn
  (λ (exn (message "")) (gdp:push-error (string-append message (exn->string exn)))))


(defprovide gd-invoke
  (lambda (obj method-name arg-list)
      ; (displayln (format "gd-invoke ~a ~a" method-name arg-list))
      ; (flush-output)
      (apply dynamic-send obj method-name arg-list)))


(defprovide Input
  (gdp:get-singleton "Input"))


; TODO: Parameterize these maybe?
(defprovide gd-type:nil 0)
(defprovide gd-type:bool 1)
(defprovide gd-type:int 2)
(defprovide gd-type:float 3)
(defprovide gd-type:string 4)
(defprovide gd-type:vector2 5)
(defprovide gd-type:vector2i 6)
(defprovide gd-type:rect2 7)
(defprovide gd-type:rect2i 8)
(defprovide gd-type:vector3 9)
(defprovide gd-type:vector3i 10)
(defprovide gd-type:transform2d 11)
(defprovide gd-type:vector4 12)
(defprovide gd-type:vector4i 13)
(defprovide gd-type:plane 14)
(defprovide gd-type:quaternion 15)
(defprovide gd-type:aabb 16)
(defprovide gd-type:basis 17)
(defprovide gd-type:transform3d 18)
(defprovide gd-type:projection 19)
(defprovide gd-type:color 20)
(defprovide gd-type:string-name 21)
(defprovide gd-type:node-path 22)
(defprovide gd-type:rid 23)
(defprovide gd-type:object 24)
(defprovide gd-type:callable 25)
(defprovide gd-type:signal 26)
(defprovide gd-type:dictionary 27)
(defprovide gd-type:array 28)
(defprovide gd-type:packed-byte-array 29)
(defprovide gd-type:packed-int32-array 30)
(defprovide gd-type:packed-int64-array 31)
(defprovide gd-type:packed-float32-array 32)
(defprovide gd-type:packed-float64-array 33)
(defprovide gd-type:packed-string-array 34)
(defprovide gd-type:packed-vector2-array 35)
(defprovide gd-type:packed-vector3-array 36)
(defprovide gd-type:packed-color-array 37)
(defprovide gd-type:variant-max 38)

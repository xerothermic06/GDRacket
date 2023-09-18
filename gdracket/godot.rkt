#lang racket

(require
  racket/base
  racket/exn
  (prefix-in gdp: 'gd-primitive))


(define-syntax (defprovide stx)
  (let* ([stx-list (syntax->list stx)]
         [sym (cadr stx-list)]
         [expr (caddr stx-list)])
  (datum->syntax stx `(begin (define ,sym ,expr) (provide (prefix-out gd: ,sym))))))


(define-syntax def-gd
  (syntax-rules()
    [(provide-gd id expr) (begin (define id expr) (provide (prefix-out gd: id)))]
    [(provide-gd (struct id rest ...)) (begin (provide (prefix-out gd: (struct-out id))) (struct id rest ...)) ]))


(define-syntax-rule (values->list EXPR)
  (call-with-values (λ () EXPR) list))


; (provide (struct-out gd-prop))
(def-gd (struct prop (name type) #:transparent #:reflection-name 'gd:prop))


; (provide (struct-out method))
(def-gd (struct method (name return-type arguments) #:transparent #:reflection-name 'gd:method))


; (provide (struct-out class))
(def-gd (struct class (name methods properties) #:transparent #:reflection-name 'gd:class))


(def-gd print
  (λ ... (displayln ...) (flush-output)))


(def-gd push-error
  (λ ... (gdp:push-error (apply string-append ...))))


(def-gd push-exn
  (λ (exn (message "")) (gdp:push-error (string-append message (exn->string exn)))))


(def-gd gd-invoke
  (lambda (obj method-name arg-list)
      (apply dynamic-send obj method-name arg-list)))


(def-gd Input
  (gdp:get-singleton "Input"))


; TODO: Parameterize these maybe?
(def-gd type:nil 0)
(def-gd type:bool 1)
(def-gd type:int 2)
(def-gd type:float 3)
(def-gd type:string 4)
(def-gd type:vector2 5)
(def-gd type:vector2i 6)
(def-gd type:rect2 7)
(def-gd type:rect2i 8)
(def-gd type:vector3 9)
(def-gd type:vector3i 10)
(def-gd type:transform2d 11)
(def-gd type:vector4 12)
(def-gd type:vector4i 13)
(def-gd type:plane 14)
(def-gd type:quaternion 15)
(def-gd type:aabb 16)
(def-gd type:basis 17)
(def-gd type:transform3d 18)
(def-gd type:projection 19)
(def-gd type:color 20)
(def-gd type:string-name 21)
(def-gd type:node-path 22)
(def-gd type:rid 23)
(def-gd type:object 24)
(def-gd type:callable 25)
(def-gd type:signal 26)
(def-gd type:dictionary 27)
(def-gd type:array 28)
(def-gd type:packed-byte-array 29)
(def-gd type:packed-int32-array 30)
(def-gd type:packed-int64-array 31)
(def-gd type:packed-float32-array 32)
(def-gd type:packed-float64-array 33)
(def-gd type:packed-string-array 34)
(def-gd type:packed-vector2-array 35)
(def-gd type:packed-vector3-array 36)
(def-gd type:packed-color-array 37)
(def-gd type:variant-max 38)

#lang racket
; Root Godot interface module to require from script classes.

(require
  (for-syntax syntax/parse)
  racket/exn)


(define-syntax (require-provide-all-out stx)
  (syntax-parse stx
    [(require-provide-all-out module-res:expr)
      #'(begin (require module-res) (provide (all-from-out module-res)))]))


; TODO: contracts for procs
(require-provide-all-out "./gd-script-classes.rkt")
(require-provide-all-out "./gd-builtins.rkt")
(require-provide-all-out "./gd-native-interface.rkt")
(require-provide-all-out "./gd-util.rkt")

(define (get-gdprimitive-proc proc-symbol)
  (dynamic-require ''gd-primitive proc-symbol))


(define-syntax def-gd
  (syntax-rules()
    [(provide-gd id expr) (begin (define id expr) (provide id))]
    [(provide-gd (struct id rest ...)) (begin (provide (struct-out id)) (struct id rest ...)) ]))


(def-gd print (λ ... (displayln ...) (flush-output)))


(def-gd push-error (λ ...
  ((get-gdprimitive-proc 'push-error) (apply string-append ...))))


(def-gd push-exn (λ (exn (message ""))
  ((get-gdprimitive-proc 'push-error) (string-append message (exn->string exn)))))


(def-gd invoke
  (lambda (obj method-name arg-list)
      (apply dynamic-send obj method-name arg-list)))

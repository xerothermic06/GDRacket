#lang racket

; Module plumbing code, not for user-side usage

(require
  racket/vector
  racket/struct)

(require (for-syntax syntax/parse syntax/parse/lib/function-header))


(provide vector-map-break)
(define vector-map-break
  (lambda (vec proc break-proc)
    (letrec
      ([iter (lambda (result vec)
        (if (and
              (not (void? result))
              (or (eq? (vector-length vec) 0) (break-proc result)))
          result
          (iter (proc (vector-ref vec 0)) (vector-drop vec 1))))])
    (iter (void) vec))))


(provide read-multi)
(define read-multi
  (lambda (in)
    (letrec ([read-rest (lambda (in vec)
                          (let ([valu (read in)])
                            (if (eq? valu eof)
                                vec
                                (read-rest in (vector-append vec `#(,valu) )))))])
      (read-rest in #() ))))


(provide app->result)
(define-syntax app->result
  (syntax-rules ()
    [(app->result exprs ...)
      (with-handlers
        ([void
          (lambda (exn)
            (displayln (continuation-mark-set->context (exn-continuation-marks exn)))
            (displayln (exn-message exn))
            (flush-output)
            (cons #f exn))])
          (cons #t (begin exprs ...)))
      ]))


; Root eval function for all calls into Racket from native library. Simplifies
; unexpected exception handling.
(provide eval-handle)
(define eval-handle
  (lambda (sexpr (name-space (current-namespace)))
      (with-handlers ([void (lambda (exn)
                      (displayln (continuation-mark-set->context (exn-continuation-marks exn)))
                      (displayln (exn-message exn))
                      (flush-output)
                      (cons #f exn))])
          (cons #t (eval sexpr name-space)))))


(provide eval-source)
(define eval-source
  (lambda (source-str module-name name-space)
;    (displayln "eval-source")
;    (displayln module-name)
;    (flush-output)
    (parameterize
      ([read-accept-lang #t]
        [read-accept-reader #t]
        [compile-enforce-module-constants #f]
        [current-module-declare-name (make-resolved-module-path module-name)])
      (let ([exprs-vec         (read-multi (open-input-string source-str))]
            [break-cond        (lambda (result-vec) (not (car result-vec)))]
            [eval-handle-in-ns (lambda (sexpr) (eval-handle sexpr name-space))])
        (vector-map-break exprs-vec eval-handle-in-ns break-cond)))))


; Read a GDRacket class file as a module and return the class info object from it.
(provide gdracket-class-register)
(define (gdracket-class-register source-str module-name name-space)
;  (displayln module-name)
;  (flush-output)
  (eval-source source-str module-name name-space)
  (define class-instance (vector-ref (dynamic-require `',module-name 'CLASS-INFO) 0))
  class-instance)


; Instantiate a gdracket object given a backing Object*.
(provide gdracket-class-instantiate)
(define (gdracket-class-instantiate module-name-sym name-space object-ptr)
  ; (eval-handle `(new (dynamic-require `',module-name-sym 'CLASS)))))
  ; (new (dynamic-require `',module-name-sym 'CLASS) (object-cptr object-cptr) )))
  (begin
    (define class-instance (vector-ref (dynamic-require `',module-name-sym 'CLASSES) 0))
    (define obj-instance (new class-instance))
    (set-field! object-ptr obj-instance object-ptr)
    obj-instance))


(provide attach-modules)
(define attach-modules
  (lambda (src-ns dest-ns module-list)
    (map
      (lambda (modul)
        (namespace-attach-module src-ns modul dest-ns))
      module-list)))


(provide struct->list-ret-false)
(define struct->list-ret-false
  (lambda (struct-inst) (struct->list struct-inst #:on-opaque 'return-false)))


(provide print-cptr)
(define print-cptr
  (lambda (cptr)
    (eval-handle `(display ,cptr))))


; The following procedures permit calling into GDRacket's native procedures by
; dynamic-requiring them from a primitive module named gdprimitive and caching
; them for later use.

; 
(define (gdprimitive-get-proc-cache proc-name)
  (define proc '())
  (λ args
    (cond
      [(procedure? proc) (apply proc args)]
      [else
       (begin
         (set! proc (dynamic-require ''gd-primitive proc-name))
         (apply proc args))])))

(define-syntax (gdprimitive-def-provide-cache stx)
  
  (syntax-parse stx
    [(_ name:id gdp-name:id signature-args:expr (~optional (gdp-invocation:id ...)))
     (begin
       (define proc-name (syntax->datum #'name))
       (define gdprimitive-proc-name (syntax->datum (attribute gdp-name)))
       (define cache-name (string->symbol (format "~a-cache" gdprimitive-proc-name)))
       (define sig-args-list (syntax->datum #'signature-args))
       (define gdp-invocation-list
         (cond
           [(not (attribute gdp-invocation)) sig-args-list]
           [else (syntax->datum #'(gdp-invocation ...))]))
       (define result
         `(begin
            (define ,cache-name (gdprimitive-get-proc-cache (quote ,gdprimitive-proc-name)))
            (provide ,proc-name)
            (define (,proc-name ,@sig-args-list) (,cache-name ,@gdp-invocation-list))))
       (datum->syntax stx result)
       )]))


(gdprimitive-def-provide-cache gdobject? gdobject? (obj))
(gdprimitive-def-provide-cache geti geti (obj idx))
(gdprimitive-def-provide-cache seti! seti! (obj idx valu))
(gdprimitive-def-provide-cache call call (obj mthd . args) (obj mthd args))
(gdprimitive-def-provide-cache instantiate-object instantiate (class-symbol))
(gdprimitive-def-provide-cache get-singleton get-singleton (class-symbol))


;(define geti-cache (gdprimitive-get-proc-cache 'geti))
;(provide geti)
;(define (geti obj idx) (geti-cache obj idx))

;(define seti!-cache (gdprimitive-get-proc-cache 'seti))
;(provide seti!)
;(define (seti! obj idx valu) (seti!-cache obj idx valu))

;(define call-cache (gdprimitive-get-proc-cache 'call))
;(provide call)
;(define (call obj mthd . args) (call-cache obj mthd args))

;(define instantiate-cache (gdprimitive-get-proc-cache 'instantiate))
;(provide instantiate-object)
;(define (instantiate-object class-symbol) (instantiate-cache class-symbol))

;(define get-singleton-cache (gdprimitive-get-proc-cache 'get-singleton))
;(provide get-singleton)
;(define (get-singleton class-symbol) (get-singleton-cache class-symbol))


(define setget-target/c (or/c gdobject?))
(define setget-index/c (or/c symbol? integer?))

(provide (contract-out
  [get (-> setget-target/c setget-index/c any)]))
(define get
  (lambda (obj-or-vnt nm)
;    (displayln (format "get: ~a ~a ~a" obj-or-vnt (gdp:object? obj-or-vnt) nm))
;    (flush-output)
    (cond [#t ;[(gdp:object? obj-or-vnt)
      (if (integer? nm)
        (geti obj-or-vnt nm)
        (call obj-or-vnt "get" (symbol->string nm)))])))


(provide (contract-out
  [set (-> setget-target/c setget-index/c any/c boolean?)]))
(define set
  (lambda (obj-or-vnt nm valu)
;    (displayln (format "get: ~a ~a ~a" obj-or-vnt (gdp:object? obj-or-vnt) nm))
;    (flush-output)
    (cond
      [#t ;(gdp:object? obj-or-vnt)
        (if (integer? nm)
          (seti! obj-or-vnt nm valu)
          (call obj-or-vnt "set" (symbol->string nm) valu))])
      #f))

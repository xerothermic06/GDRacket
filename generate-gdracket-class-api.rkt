#lang racket

(require json)
(require fmt)

;(require "./gdracket/gd-script-classes.rkt")


(define (godot-name->racket-name str)
  (string-replace (format "~a" str) "_" "-"))

(define (godot-field->getter-fn-symbol str)
  (string->symbol (format "get-~a" (godot-name->racket-name str))))

(define (godot-field->setter-fn-symbol str)
  (string->symbol (format "set!-~a" (godot-name->racket-name str))))

(provide class-info->class-expression)
(define (class-info->class-expression info-dict)
  (begin
    (define class-properties (hash-ref info-dict 'properties '()))
    (define class-methods (hash-ref info-dict 'methods '()))
    (define super-class-object (string->symbol (hash-ref info-dict 'inherits "godot-object-base"))))

    (define method-clauses
      (for/list ([method-hash class-methods])
        (define method-name (string->symbol (hash-ref method-hash 'name)))
        (define rkt-method-name (string->symbol (godot-name->racket-name method-name)))
        (define arglist
          (for/list ([arg-hash (hash-ref method-hash 'arguments '())]) (string->symbol (hash-ref arg-hash 'name))))
        `(define/public (,rkt-method-name ,@arglist) (gdobject-invoke ',method-name ,@arglist))))

    (define field-clauses
      (for/fold ([lst '()]) ([prop-hash class-properties])
        (define prop-name-sym (string->symbol (hash-ref prop-hash 'name)))
        (list* `(define/final (,(godot-field->getter-fn-symbol prop-name-sym)) (gdobject-get ',prop-name-sym))
               `(define/final (,(godot-field->setter-fn-symbol prop-name-sym) valu) (gdobject-set ',prop-name-sym valu))
               lst)))
;        `(field (,(string->symbol (hash-ref prop-hash 'name)) '()))))

    `(class ,super-class-object (super-new) ,@field-clauses ,@method-clauses))


(define gdextension-method->datum
  (λ (jsexpr)
    (let
        ([method-name (hash-ref jsexpr 'name)]
         [method-args (hash-ref jsexpr 'arguments '())])
      `(,(string->symbol method-name) ,method-args))))


(define (flattenn lst ct)
  (letrec ([flatn
            (λ (lst)
              (vector->list
               (foldl (λ (lst accu-vec)
                        (vector-append (list->vector lst)
                                       accu-vec))
                      #()
                      lst)))]
           [loop
            (λ (lst ct)
              (if (= ct 0)
                  lst
                  (loop (flatn lst) (- ct 1))))])
    (loop lst ct)))


(define (flatten-once lst-of-lsts)
  (vector->list (foldl (λ (lst accu-vec) (vector-append (list->vector lst) accu-vec)) #() lst-of-lsts)))


(define (gdextension-class->enum-data class-jsexpr)
  (let*
      ([class-name (string->symbol (hash-ref class-jsexpr 'name))]
       [value-mapper-gen
        (λ (enum-name)
          (λ (value-hash)
            (let ([enum-value-name
                   (string->symbol
                    (format "~a:~a:~a"
                            class-name enum-name
                            (hash-ref value-hash 'name)))])
              `((provide ,enum-value-name)
                (define-syntax (,enum-value-name stx)
                  (syntax ,(hash-ref value-hash 'value)))))))]

       [enum-mapper
        (λ (enum-hash)
          (map (value-mapper-gen (hash-ref enum-hash 'name))
               (hash-ref enum-hash 'values)))])
    (map enum-mapper (hash-ref class-jsexpr 'enums '()))))


(define (gdextension-class->method-data class-jsexpr)
  (let*
      ([methods-lst (hash-ref class-jsexpr 'methods '())]
       [argument-mapper
        (λ (argument-hash)
          (string->symbol (hash-ref argument-hash 'name)))]
       [method-mapper
        (λ (method-hash)
          `(,(string->symbol (hash-ref method-hash 'name))
            ,@(map argument-mapper (hash-ref method-hash 'arguments '()))))])
    (map method-mapper (hash-ref class-jsexpr 'methods '()))))


(define gdextension-class->datum
  (λ (jsexpr)
    (let*
        ([class-name (string->symbol (hash-ref jsexpr 'name))]

         [inherits (string->symbol (hash-ref jsexpr 'inherits "object%"))]

         [methods (gdextension-class->method-data jsexpr)]
;          (map
;           (λ (method-hash) (string->symbol (hash-ref method-hash 'name)))
;           (hash-ref jsexpr 'methods '()))]
         
         [properties (map
                      (λ (prop-hash) (string->symbol (hash-ref prop-hash 'name)))
                      (hash-ref jsexpr 'properties '()))]
         
         [enums (flattenn (gdextension-class->enum-data jsexpr) 2)]
         [extra-exprs
          (match class-name
            ['Object '((godot-object-mixin))]
            [_ '()])])

      `(begin
         (provide ,class-name)
         (define ,class-name ,(class-info->class-expression jsexpr))
         ,@enums))))
;         (define ,class-name (class ,inherits (super-new) ,methods ,properties ,@extra-exprs) ,@enums)))))
;         (gd-api-class ,class-name ,inherits ,methods ,properties ,@extra-exprs) ,@enums))))



(define gdextension-interface-classes->datum
  (λ (jsexpr)
    (let
        ([gd-classes (hash-ref jsexpr 'classes)])
      (map gdextension-class->datum
;           (vector->list (vector-take (list->vector gd-classes) 10))
;           gd-classes
           (filter (λ (gd-class) (vector-member (hash-ref gd-class 'name) #("RefCounted" "Object" "Node"))) gd-classes)
           ))))


(define file-header
  '(begin (require "./gd-object-base.rkt")))


(define (print-usage-exit)
  (display "usage: gd-api-gen.rkt <input-file> <output-file>")
  (exit 1))


(define (main)
  (if (< (vector-length (current-command-line-arguments)) 2)
      (print-usage-exit)
      (let*
          ([clarg-ref (λ (n) (vector-ref (current-command-line-arguments) n))]
           [input-file (clarg-ref 0)]
           [output-file (clarg-ref 1)]
           [input-file-port (open-input-file input-file)]
           [file-json (read-json input-file-port)]
           [output-expr (gdextension-interface-classes->datum file-json)]
           [output-str-port (open-output-string)]
           [output-str ""]
           [write-out-proc
;            (λ (out)
;              (map (λ (sexpr) (begin (write sexpr out) (write #\newline out))) output-expr))]
            (λ (out) (display output-str out))]
           )

        (displayln "#lang racket" output-str-port)
        (write file-header output-str-port)
        (map (λ (sexpr) (write sexpr output-str-port)) output-expr)
        
        (set! output-str (program-format (get-output-string output-str-port)))
        (call-with-output-file output-file #:exists 'truncate write-out-proc))))

;(gdextension-method->datum
; (read-json (open-input-string "{\"name\": \"foo\", \"arguments\": [{\"name\": \"foo\", \"type\": \"int\"}]}")))

(parameterize
 ([current-directory "D:/projects/godot/gdracket"]
  [current-command-line-arguments #("godot-cpp/gdextension/extension_api.json" "gdracket/gd-class-api.rkt")])
 (main))

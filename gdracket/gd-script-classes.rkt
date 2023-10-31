#lang racket

(require (for-syntax syntax/parse))
(require (prefix-in gdp: "./gd-native-interface.rkt"))
(require "./gd-object-base.rkt")
(require "./gd-util.rkt")

; Script class components ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (any->symbol arg) (string->symbol (format "~a" arg)))


; Generate an s-expression for a Godot wrapper class based on a dictionary with
; the class' info.
(provide class-info->class-expression)
(define (class-info->class-expression info-dict)
  (begin
    (define class-properties (hash-ref info-dict 'properties '()))
    (define class-methods (hash-ref info-dict 'methods '()))
    (define super-class-object
      (_get-api-class (any->symbol (hash-ref info-dict 'parent))))

    (define method-clauses
      (for/list ([method-hash class-methods])
        (define method-name (any->symbol (hash-ref method-hash "name")))
        (define arglist
          (for/list ([arg-hash (hash-ref method-hash "args" '())]) (any->symbol (hash-ref arg-hash "name"))))
        `(define/public (,method-name ,@arglist) (delegate-invoke ',method-name ,@arglist))))

    (define field-clauses
      (for/list ([prop-hash class-properties])
        `(field (,(any->symbol (hash-ref prop-hash "name")) '()))))

    `(class ,super-class-object (super-new) ,@field-clauses ,@method-clauses)))


; When true, class wrappers for builtin Godot types will be generated dynamically;
; otherwise, they will be required from gd-class-api.
(define dynamic-wrapper-classes (make-parameter #t))

; Gets a class that wraps a builtin Godot type based on its exports. These definitions are
; generated dynamically and cached on subsequent calls.
(provide get-api-class)
(define (get-api-class class-name-symbol)
  (cond
    [dynamic-wrapper-classes
     (define result (gdp:app->result (_get-api-class class-name-symbol)))
     (log/info result)
     (cond [(car result) (cdr result)] [else (void)])]
    [else
     (dynamic-require "./gd-class-api.rkt" class-name-symbol)]))


(define api-class-store (make-hash))
(define (_get-api-class class-name-symbol)
  (log/info class-name-symbol)
  (log/info (hash-has-key? api-class-store class-name-symbol))
  (cond

    [(hash-has-key? api-class-store class-name-symbol)
     (hash-ref api-class-store class-name-symbol)]
    
    [else
     (define info-dict
       ((dynamic-require ''gd-primitive 'get-api-class-info) class-name-symbol))

     (define class-properties (hash-ref info-dict 'properties '()))
     (define class-methods (hash-ref info-dict 'methods '()))
     
     (log/info class-name-symbol (for/list ([method-hash class-methods]) (hash-ref method-hash "name")))
     
     (define super-class-object
       (let ([super-class-name (hash-ref info-dict 'parent (void))])
         (begin
           (log/info class-name-symbol "extends" super-class-name)
           (cond
             [(or (equal? class-name-symbol 'Object) (void? super-class-name)) godot-object-base]
             [else (_get-api-class (any->symbol super-class-name))]))))

;     (log/info (format "class name ~a superclass ~a" (hash-ref info-dict 'parent (void)) super-class-object))
     
;       (_get-api-class (any->symbol (hash-ref info-dict 'parent))))

     (define method-clauses
       (for/list ([method-hash class-methods])
         (define method-name (any->symbol (hash-ref method-hash "name")))
         (define arglist
           (for/list ([arg-hash (hash-ref method-hash "args" '())]) (any->symbol (hash-ref arg-hash "name"))))
         `(define/public (,method-name ,@arglist) (delegate-invoke ',method-name ,@arglist))))

     (define field-clauses
       (for/list ([prop-hash class-properties])
         `(field (,(any->symbol (hash-ref prop-hash "name")) '()))))
     
     (define api-class-wrapper
       (let ([wrapper-expr `(class ,super-class-object (super-new) ,@field-clauses ,@method-clauses)])
         (eval wrapper-expr)))
     
     (hash-set! api-class-store class-name-symbol api-class-wrapper)
     
     api-class-wrapper]))


; Script class helper macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Creates toplevel definitions for CLASSES and CLASS-INFO vectors that are read by GDRacket
; to get exported class information.
(provide register-classes)
(define-syntax (register-classes stx)
  (syntax-parse stx
    [((~literal register-classes) ~! ids:id ...)
     (begin
       (define info-id-symbols
         (datum->syntax stx
                        (for/list
                            ([cls-id-stx (syntax->list #'(ids ...))])
                          (string->symbol (format "~a-info" (syntax->datum cls-id-stx))))))
       (define-values (CLASSES-stx CLASS-INFO-stx)
         (apply values (for/list ([sym '(CLASSES CLASS-INFO)]) (datum->syntax stx sym))))
       #`(begin
           (provide #,CLASSES-stx)
           (define #,CLASSES-stx (vector ids ...))
           (provide #,CLASS-INFO-stx)
           (define #,CLASS-INFO-stx (vector #,@info-id-symbols))))]))


(define-for-syntax symbol-type-map
  #hash(
    (Variant . 0)
    (bool . 1)
    (int . 2)
    (float . 3)
    (String . 4)
    (Vector2 . 5)
    (Vector2i . 6)
    (Rect2 . 7)
    (Rect2i . 8)
    (Vector3 . 9)
    (Vector3i . 10)
    (Transform2D . 11)
    (Vector4 . 12)
    (Vector4i . 13)
    (Plane . 14)
    (Quaternion . 15)
    (Aabb . 16)
    (Basis . 17)
    (Transform3d . 18)
    (Projection . 19)
    (Color . 20)
    (StringName . 21)
    (NodePath . 22)
    (Rid . 23)
    (Object . 24)
    (Callable . 25)
    (Signal . 26)
    (Dictionary . 27)
    (Array . 28)
    (PackedByteArray . 29)
    (PackedInt32Array . 30)
    (PackedInt64Array . 31)
    (PackedFloat32Array . 32)
    (PackedFloat64Array . 33)
    (PackedStringArray . 34)
    (PackedVector2Array . 35)
    (PackedVector3Array . 36)
    (PackedColorArray . 37)
    (VariantMax . 38)))


(define-for-syntax (arg-type->type-int sym)
  ; (displayln sym)
  (if (null? sym)
      0
      (hash-ref symbol-type-map sym (hash-ref symbol-type-map 'Object))))


; gd-class generates definitions for a class expression and a class metadata hash table.
(provide gd-class)
(define-syntax (gd-class stx)

; Syntax Classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define-syntax-class signal-or-func-arg
    #:attributes (name type)
    (pattern name:id #:with type #'null)
    (pattern
;      (name:id (~optional type:id #:defaults ([type #'()])))))
       (name:id type:id)))

  (define-syntax-class signal-decl
    (pattern
      ((~literal signal)
       name:id
       (~optional (args:signal-or-func-arg ...) #:defaults ([(args 1) '()]) ))))

  (define-syntax-class export-var-decl
    #:no-delimit-cut
    #:description "export var"
    (pattern
      ((~literal export-var) ~!
       (~describe "export-var name" name:id)
       (~describe "export-var type" (type:id))
       init-value:expr)))

  (define-syntax-class var-decl
    #:description "var"
    #:no-delimit-cut
    (pattern
      ((~literal var) ~!
       (~describe "var name"
                  name:id)
       (~describe "var type"
                  (~optional (type:id) #:defaults ([type #'()])))
       (~describe "var initial value"
                  (~optional init-value:expr #:defaults ([init-value #'()]))))))

  (define-splicing-syntax-class func-return-type
    #:attributes (type)
    (pattern (~seq (~literal ->) type:id)))

  (define-syntax-class func-keyword
    #:attributes (define-variant)
    (pattern (~literal func) #:attr define-variant #'define/public)
    (pattern (~literal func/override) #:attr define-variant #'define/override)
    (pattern (~literal func/augment) #:attr define-variant #'define/augment))
  
  ; Syntax class for func
  (define-syntax-class func-decl
    #:no-delimit-cut
    #:description "function declaration"
    #:auto-nested-attributes
    (pattern
      (func-kw:func-keyword ~!
       ;(~literal func) ~!
       name:id
       (args:signal-or-func-arg ...)
       (~optional return-type:func-return-type #:defaults ([return-type.type #'()]))
       func-proc:expr ...)))

  (define-syntax-class gd-class-decl
    #:description "class declaration"
    (pattern (~literal gd-class)))

  ; Syntax class for any class clause
  (define-syntax-class class-clause
    #:description "class clause"
    (pattern (~or vr:var-decl fnc:func-decl expv:export-var-decl cls:expr)))

  
  ; Helper Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ; convert a list of (arg-name type) or arg-name forms into a list of hash tables
  (define (args-to-hashes args-list)
    (for/list ([el args-list])
      (cond
        [(list? el) `#hash((name . ,(car el)) (type . ,(arg-type->type-int (cadr el))))]
        [else `#hash((name . ,el) (type . 0))])))
  
  ; extracts metadata attributes from gd-class subforms
  (define (export-syntax-get-attrs stx)
    (let
        ([parse-result
          (syntax->datum
           (syntax-parse stx
             [fd:func-decl
              (let*
                  ([arg-hashs (args-to-hashes (syntax->datum #'(fd.args ...)))]
                   [return-type (arg-type->type-int (syntax->datum #'fd.return-type.type))])
                #`((kind . func) (name . fd.name) (args . #,arg-hashs) (return . #,return-type)))]
             [vr:var-decl
              (let* ([arg-type (arg-type->type-int (syntax->datum #'vr.type))])
                #`((kind . var) (name . vr.name) (type . #,arg-type)))]
             [exvr:export-var-decl
              (let* ([arg-type (arg-type->type-int (syntax->datum #'vr.type))])
                #`((kind . export-var) (name . exvr.name) (type . #,arg-type)))]
             [sig:signal-decl
              (let* ([arg-hashs (args-to-hashes (syntax->datum #'(sig.args ...)))])
                #`((kind . signal) (name . sig.name) (args . #,arg-hashs)))]
             [_ #'()]))])
      (if (null? parse-result) parse-result (make-hash parse-result))))

  ; base transformer for each individual gd-class clause
  (define export-syntax-transform
    (λ (stx)
      (syntax->datum
       (syntax-parse stx
;         [fd:func-decl #'(define/public (fd.name fd.args.name ...) fd.func-proc ...)]
         [fd:func-decl #'(fd.func-kw.define-variant (fd.name fd.args.name ...) fd.func-proc ...)]
         [vr:var-decl #'(field (vr.name vr.init-value))]
         [expv:export-var-decl #'(field (expv.name expv.init-value))]
         [_ #'()]))))

  ; parse-class-clauses iterates over a list of class-clause syntax objects
  ; and returns a two-element list; first element is metadata for exported variables and methods, second
  ; list is the input list with clauses removed or transformed
  (define parse-class-clauses
    (λ (syntax-list)
      (let loop
        ([var-meta-list '()] ; list of export-var metadata
         [stx-out-list '()] ; list of output syntax
         [rest syntax-list]) ; iterator
        (if (null? rest)
            (list var-meta-list (reverse stx-out-list))
            (let*
                ([class-clause (car rest)]
                 [export-var-attrs (export-syntax-get-attrs class-clause)]
                 [transformed-stx (export-syntax-transform class-clause)]
                 [new-stx-list
                  (cond
                    [(and (null? transformed-stx) (null? export-var-attrs)) (cons class-clause stx-out-list)]
                    [(null? transformed-stx) stx-out-list]
                    [else (cons transformed-stx stx-out-list)])]
                 [new-metas-list
                  (cond
                    [(null? export-var-attrs) var-meta-list]
                    [else (cons export-var-attrs var-meta-list)])])
              (loop
               new-metas-list
;               (if skip-clause var-meta-list (cons export-var-attrs var-meta-list))
;               (if skip-clause (cons (car rest) stx-out-list) stx-out-list)
               new-stx-list
               (cdr rest)))))))

  (define (hash/group-by lst classifier)
    (for/fold ([table #hash()]) ([itm lst])
      (define key (classifier itm))
      (define new-value
        (cond
          [(hash-has-key? table key) (cons itm (hash-ref table key))]
          [else (list itm)]))
      (hash-set table key new-value)))
  
;  (define hash/group-by
;    (λ (lst classifier)
;      (foldl
;       (λ (itm table)
;         (let*
;             ([key (classifier itm)]
;              [new-value (if (hash-has-key? table key)
;                             (cons itm (hash-ref table key))
;                             (list itm))])
;           (hash-set table key new-value)))
;       #hash()
;       lst)))

  
  ; Base Syntax Parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (syntax-parse stx
    [(class-decl:gd-class-decl class-name:id super-expr:expr cls:class-clause ...)
     (let*
         ([clauses
           (syntax->list #'(cls ...))]

          [class-nm (syntax->datum #'class-name)]

          [class-info-name (string->symbol (format "~a-info" class-nm))]

          [var-metas-and-rest-clauses
           (parse-class-clauses clauses)]

          [metadata-hash
           (hash-set (hash/group-by
                      (car var-metas-and-rest-clauses)
                      (λ (table-itm) (hash-ref table-itm 'kind)))
                     'name
                     class-nm)]
          
;          [var-metas-syntax
;           (datum->syntax
;            stx
;            (hash-set (hash/group-by
;                       (car var-metas-and-rest-clauses)
;                       (λ (table-itm) (hash-ref table-itm 'kind)))
;                      'name
;                      class-nm))]

;          [class-metas-name-syntax
;           (datum->syntax
;            stx
;            (string->symbol
;             (format "~a:info" (syntax->datum #'class-name))))]

          [rest-clauses
           (cadr var-metas-and-rest-clauses)]

;          [metas-define
;           #`(define #,class-metas-name-syntax #,var-metas-syntax)]

          [super-xpr (syntax->datum #'super-expr)]

          [class-expr
           (datum->syntax stx
                          `(begin
                             (provide ,class-nm)
                             (define ,class-nm (class ,super-xpr ,@rest-clauses))))]

          [class-info-expr
           (datum->syntax
            stx
            `(begin
               (provide ,class-info-name)
               (define ,class-info-name ,metadata-hash)))]) ;,(syntax->datum class-metas-name-syntax))))])

       (displayln metadata-hash) (flush-output)

       #`(begin #,class-expr #,class-info-expr))]))



;; Macro to generate class definitions for builtin Godot classes. Not to be used by user scripts.
;(provide gd-api-class)
;(define-syntax (gd-api-class stx)
;
;  (define-syntax-class method-decl
;    #:description "method declaration"
;    #:no-delimit-cut
;    (pattern
;      ((~describe "method name" name:id)
;       (~describe "method args" args:id) ...)))
;
;  (define-syntax-class field-decl
;    #:description "field declaration"
;    #:no-delimit-cut
;    (pattern
;      (~describe "field name" name:id)))
;
;  (define (tf-decl decl-stx)
;    (syntax->datum
;     (syntax-parse decl-stx
;       [md:method-decl #'(define/public (md.name md.args ...) (void))]
;       [fd:field-decl #'(field (fd.name (void)))])))
;
;  (syntax-parse stx
;    [(gd-api-class ~! class-name:id super-name:id (method-decls:method-decl ...) (field-decls:field-decl ...) cls-stmts:expr ...)
;     (let*
;         ([method-clauses (map tf-decl (syntax->list #'(method-decls ...)))]
;          [field-clauses (map tf-decl (syntax->list #'(field-decls ...))) ]
;          [class-name (syntax->datum #'class-name)]
;          [super-name (let ([name-res (syntax->datum #'super-name)])
;                        (if (string=? (symbol->string name-res) "")
;                            'godot-object-mixin
;                            name-res))]
;          [output-stx
;           (datum->syntax
;            stx
;            `(define ,class-name
;               (class ,super-name ,@field-clauses ,@method-clauses ,@(syntax->datum #'(cls-stmts ...)))))])
;       output-stx)]))

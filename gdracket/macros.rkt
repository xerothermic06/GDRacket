#lang racket

;(require (for-syntax racket/class))
(require (for-syntax syntax/parse))


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
  
  ; Syntax class for export-var
  (define-syntax-class export-var-decl
    #:no-delimit-cut
    #:description "export var"
    ;    (pattern ((~literal export-var) name:id (type:id)))

    (pattern
      ((~literal export-var) ~!
       (~describe "export-var name" name:id)
       (~describe "export-var type" (type:id))
       init-value:expr)))

  ; Syntax class for var
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

  ; Syntax class for func
  (define-syntax-class func-decl
    #:no-delimit-cut
    #:description "function declaration"
    #:auto-nested-attributes
    (pattern
      ((~literal func) ~!
       name:id
       (args:signal-or-func-arg ...)
       (~optional return-type:func-return-type #:defaults ([return-type.type #'()]))
       func-proc:expr ...)))

  ; convert a list of (arg-name type) or arg-name forms into a list of hash tables
  (define args-to-hashes
    (λ (args-list)
      (map
       (λ (el)
         (cond
           [(list? el) `#hash((name . ,(car el)) (type . ,(arg-type->type-int (cadr el))))]
           [else `#hash((name . ,el) (type . 0))]))
         args-list)))

  ; extracts metadata attributes from gd-class subforms
  (define export-syntax-get-attrs
    (λ (stx)
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
        (if (null? parse-result) parse-result (make-hash parse-result)))))

  (define export-syntax-transform
    (λ (stx)
      (syntax->datum
       (syntax-parse stx
         [fd:func-decl #'(define/public (fd.name fd.args.name ...) fd.func-proc ...)]
         [vr:var-decl #'(field (vr.name vr.init-value))]
         [expv:export-var-decl #'(field (expv.name expv.init-value))]
         [_ #'()]))))

  ; Syntax class for any class clause
  (define-syntax-class class-clause
    #:description "class clause"
    (pattern (~or vr:var-decl fnc:func-decl expv:export-var-decl cls:expr)))

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

  (define hash/group-by
    (λ (lst classifier)
      (foldl
       (λ (itm table)
         (let*
             ([key (classifier itm)]
              [new-value (if (hash-has-key? table key)
                             (cons itm (hash-ref table key))
                             (list itm))])
           (hash-set table key new-value)))
       #hash()
       lst)))

  (syntax-parse stx
    [(_ class-name:id super-expr:expr cls:class-clause ...)
     (let*
         ([clauses
           (syntax->list #'(cls ...))]

          [class-nm (syntax->datum #'class-name)]

          [var-metas-and-rest-clauses
           (parse-class-clauses clauses)]

          [var-metas-syntax
           (datum->syntax
            stx
            (hash-set
             (hash/group-by
              (car var-metas-and-rest-clauses)
              (λ (table-itm) (hash-ref table-itm 'kind)))
             'name
             class-nm))]

          [class-metas-name-syntax
           (datum->syntax
            stx
            (string->symbol
             (format "~a:info" (syntax->datum #'class-name))))]

          [rest-clauses
           (cadr var-metas-and-rest-clauses)]

          [metas-define
           #`(define #,class-metas-name-syntax #,var-metas-syntax)]

          [super-xpr (syntax->datum #'super-expr)]

          [class-expr
;           #`(define class-name (class super-expr #,@rest-clauses))])
           (datum->syntax stx `(define ,class-nm (class ,super-xpr ,@rest-clauses)))]

          [class-info-expr
           (datum->syntax
            stx
            `(begin
               (provide CLASS)
               (define CLASS ,(syntax->datum #'class-name))
               (provide CLASS_INFO)
               (define CLASS_INFO ,(syntax->datum class-metas-name-syntax))))])
       (displayln class-expr)
       #`(begin #,metas-define #,class-expr #,class-info-expr))]))



;
; a-metas

; (send (new a) untyped-func 1)



;(begin-for-syntax
;  (define-splicing-syntax-class x-mid
;    #:attributes (a b)
;    (pattern (~seq a:id (~literal X) b:id)))
;  (datum->syntaxx
;   #'afXbff
;   (syntax-parse #'(afXbff)
;     [(~var xxx x-mid) (begin (displayln #'xxx.a) #'xxx.a) ])))


; Get all the children of a Node that return true when applied to the given predicate.
;(provide get-children-filter)
;(define get-children-filter
;  (λ (prnt-node predicate)
;    (letrec
;        ([add-node-if
;          (λ (pred)
;            (λ (node result) (if (pred) (cons node result) result)))]
;         [filter-add ; return a list with the node added if predicate; else return the list
;          (add-node-if predicate)]
;         [append-children ; return list with the node's children added if it has children; else return list
;          (add-node-if (λ (node) (call has-children node)))]
;         [loop
;          (λ (to-visit result)
;            (let ([visiting (car to-visit)])
;              (if (null? to-visit)
;                  result
;                  (loop
;                   (append-children visiting (cdr to-visit))
;                   (filter-add visiting result)))))])
;      (loop (call get-children prnt-node) '()))))

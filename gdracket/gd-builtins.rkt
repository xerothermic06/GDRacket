#lang racket

; Builtin utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "./gd-native-interface.rkt")

(require (for-syntax syntax/parse))

(define builtin-construct (gdprimitive-get-proc-cache 'builtin-construct))
(define builtin-operator (gdprimitive-get-proc-cache 'builtin-operator))

; TODO: Alternate version that doesn't do dynamic-require for runtime
(define-syntax (provide-builtin-predicates stx)
  (syntax-parse stx
    [(_ builtin-pred-name:id ...)
     (let* ([predicate-names (map (λ (stxx) (syntax->datum stxx))
                                  (syntax->list #'(builtin-pred-name ...)))]
            [mapper (λ (p-name)
                      `(begin
                         (provide ,p-name)
                         (define ,p-name
                           (λ (obj) ((dynamic-require ''gd-primitive ,p-name) obj)))))]
            [output-clauses (map mapper predicate-names)])
       (datum->syntax stx `(begin ,@output-clauses)))]))


(define-syntax (provide-builtin-ctors stx)
  (syntax-parse stx
    [(_ names:id ...)
     (begin
       (define ctor-decls
         (for/list ([name-stx (syntax->list #'(names ...))])
           (define nm (syntax->datum name-stx))
           `(begin (provide ,nm) (define (,nm . args) (apply builtin-construct args)))))
       (datum->syntax stx `(begin ,@ctor-decls)))]))


(define-syntax (provide-builtin-operators stx)
  (define-syntax-class clause (pattern (name:id op:integer)))
  (syntax-parse stx
    [(_ clauses:clause ...)
     (begin
       (define op-decls (for/list ([clause-stx (syntax->list #'(clauses ...))])
                          (syntax-parse clause-stx
                            [claus:clause (begin
                                            (define nm (syntax->datum #'claus.name))
                                            (define op (syntax->datum #'claus.op))
                                            `(begin
                                               (provide ,nm)
                                               (define (,nm a (b (void)))
                                                 (cond [(void? b) (builtin-operator ,op a)]
                                                       [else (builtin-operator ,op a b)]))))])))
       (datum->syntax stx `(begin ,@op-decls)))]))


(provide-builtin-predicates Vector2?
                            Vector2i?
                            Rect2?
                            Rect2i?
                            Vector3?
                            Vector3i?
                            Transform2D?
                            Vector4?
                            Vector4i?
                            Plane?
                            Quaternion?
                            AABB?
                            Basis?
                            Transform3D?
                            Projection?
                            Color?
                            StringName?
                            NodePath?
                            RID?
                            Object?
                            Callable?
                            Signal?
                            Dictionary?
                            Array?
                            PackedByteArray?
                            PackedInt32Array?
                            PackedInt64Array?
                            PackedFloat32Array?
                            PackedFloat64Array?
                            PackedStringArray?
                            PackedVector2Array?
                            PackedVector3Array?
                            PackedColorArray?)


(provide-builtin-ctors Vector2
                       Vector2i
                       Rect2
                       Rect2i
                       Vector3
                       Vector3i
                       Transform2D
                       Vector4
                       Vector4i
                       Plane
                       Quaternion
                       AABB
                       Basis
                       Transform3D
                       Projection
                       Color
                       StringName
                       NodePath
                       RID
                       Object
                       Callable
                       Signal
                       Dictionary
                       Array
                       PackedByteArray
                       PackedInt32Array
                       PackedInt64Array
                       PackedFloat32Array
                       PackedFloat64Array
                       PackedStringArray
                       PackedVector2Array
                       PackedVector3Array
                       PackedColorArray)


(provide-builtin-operators (op:eq  0) (op:=  0)
                           (op:neq 1) (op:!= 1)
                           (op:lt  2) (op:<  2)
                           (op:lte 3) (op:<= 3)
                           (op:gt  4) (op:>  4)
                           (op:gte 5) (op:>= 5)
                           (op:add 6) (op:+ 6)
                           (op:sub 7) (op:- 7)
                           (op:mul 8) (op:* 8)
                           (op:div 9) (op:/ 9)
                           (op:neg 10)
                           (op:pos 11)
                           (op:mod 12) (op:% 12)
                           (op:<<  13)
                           (op:>>  14)
                           (op:&   15)
                           (op:\|  16)
                           (op:^   17)
                           (op:!   18)
                           (op:and 19) (op:&& 19)
                           (op:or  20) (op:||  20)
                           (op:xor 21) (op:^^ 21)
                           (op:not 22)
                           (op:in  23))


(provide dictionary-set!)
(define (dictionary-set! dict k v) (gdprimitive-get-proc-cache 'dictionary-set!))

(provide dictionary)
(define (dictionary . pairs)
  (begin
    (define dict (Dictionary))
    (for ([pair pairs])
      (dictionary-set! dict (car pair) (cdr pair)))
    dict
  ))

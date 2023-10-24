#lang racket

; Builtin utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (for-syntax syntax/parse))

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


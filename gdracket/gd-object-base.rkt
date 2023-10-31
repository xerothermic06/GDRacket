#lang racket

(require (prefix-in gdp: "./gd-native-interface.rkt"))

; base class for all GDRacket scripts.
(provide godot-object-base)
(define godot-object-base
  (class object%
    (super-new)
    ; Scheme_GodotObject* which points to the owning Godot object
    (field (object-ptr (void)))
    (define/public (self) object-ptr)
    (define/public (gdobject-invoke mthd args ...)
      (gdp:call object-ptr mthd args))
    (define/public (gdobject-set prop valu)
      (gdp:set object-ptr prop valu))
    (define/public (gdobject-get prop)
      (gdp:get object-ptr prop))))
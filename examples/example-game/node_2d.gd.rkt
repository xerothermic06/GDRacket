#lang racket

(require (prefix-in gd: "racket/gdracket/godot.rkt"))

(gd:gd-class
 test-class (gd:get-api-class 'Node2D)
 (inspect (make-inspector (current-inspector)))
 (super-new)

 (define Input (gd:get-singleton 'Input))

 (signal hit (pos Vector2))

 (var move-node '())
 (export-var some-vec (Vector3) '())
 (export-var speed (float) 1.0)
 (export-var counter (float) 0.0)

 (var something '())

 (inherit emit_signal self)

 (func/override _init ()
                (set! something (gd:instantiate-object 'Node))
                (gd:call (gd:Signal (self) 'hit) 'connect (λ (v2) (gd:print v2))))

 (func/override _process (delta)

				(gd:log/info "delta" delta)
                (cond
                  [(gd:call Input 'is_anything_pressed)
                   (displayln "something is pressed")])

                (cond [(not (null? move-node))
                       (let*
                           ([cur-pos (gd:get move-node 'position)]
                            [new-pos (gd:op:+
                                      (gd:Vector2 (gd:get cur-pos 'x) (sin counter))
                                      (gd:Vector2))])
                         (gd:set move-node 'position new-pos))])
                (cond
                  [(> counter 1.0)
                   (emit_signal 'hit (gd:Vector2 1 2))
                   (set! counter 0.0)]
                  [else (set! counter (+ counter delta))]))

 (func get-speed () speed)

 (func display-move-node ()
       (displayln (format "move-node: ~a" move-node)))

 (func test ([arg Variant]) 0))


(gd:register-classes test-class)

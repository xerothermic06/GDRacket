# GDRacket

*GDRacket* adds [Racket](https://racket-lang.org/) as a programming language for Godot 4.x.

*NOTE: This project is a work-in-progress and is nowhere near production-ready. It is currently experimental in scope and implementation.*

## Getting Started

### Installation

Simply copy the provided DLLs and gdextension files to a folder anywhere in your project.

### File Extensions

Any file ending with `.rkt` is recognized by the plugin as a Resource object. However, by default these are considered generic resources and not scripts, allowing for collections of modules that do not need to be attached to Nodes or other objects in order to be used. To create a Racket script that is attachable to Nodes, use `.gd.rkt` as the script's file extension. This indicates to the plugin that that the file is an attachable Script resource.

# Racket Scripts

The `gd-class` macro is the main entrypoint for Racket scripts.

```racket
#lang racket

; Linking to scripts relative to project filesystem
(require "res://src/scripts/helpers.rkt")

; Linking to "global" modules
(require 'helpers)

; Exported function, available to other Racket scripts but invisible to Godot
; Get all the children of a Node that return true when applied to the given predicate.
(provide get-children-filter)
(define get-children-filter
  (λ (prnt-node predicate)
    (letrec
        ([filter-add ; return a list with the node added if predicate; else return the list
          (λ (node result)
            (if (predicate node)
                (cons node result)
                result))]
         [append-children ; return list with the node's children added if it has children; else return list
          (λ (node result)
            (if (call has-children node)
                (cons (call get-children node) result)
                result))]
         [loop
          (λ (to-visit result)
            (let ([visiting (car to-visit)])
              (if (null? to-visit)
                  result
                  (loop
                   (append-children visiting (cdr to-visit))
                   (filter-add visiting result)))))])
      (loop (call get-children prnt-node) '()))))

(gd-class MyNode Node2D
    ; Non-exported variable with no fixed type (considered as Variant)
    (var some-variable 1.0)

    ; Non-exported variable with explicit type
    (var (float) some-variable 1.0)

    ; exported variable
    (export-var (int) strength 10)

    ; exported variable with editor hints
    (export-var (float 0.0 1.0) health 1.0)

    ; exported Resource variable
    (export-var (Texture) icon '())

    ; Script function
    (func (get-sprites-recursive)
        (get-children-filter this Sprite2D?))

    ; virtual callback function with fixed-type arguments
    (func (_process (delta float))
        (set-var!
            'position.x
            (+ (get 'position.x) delta)))
```

You can use any number of `define`s or `provide` forms in attachable Racket scripts and refer to them from the class body.


## Data Conversion

Booleans, floats, integers, and strings have relatively straightforward equality between Racket and Godot. The vast majority of the builtin struct types (Vector3, AABB, pointers to Objects etc.) have direct equivalents in GDRacket and are wrapped with a minimal low-level Racket struct that adds only around 4 extra bytes per instance. The following types undergo some degree of cost-inducing conversions when sending data from one to the other:

| Racket Type | Godot Type |
|-------------|------------|
| Symbol      | StringName |
| List        | Array      |
| Hash Table  | Dictionary |

## Special Features

### Interfaces

### Mixins

### Annotations

## S-Expressions

If you want to use GDScript or another system as the primary source of logic for your project but wish to take advantage of Racket's on-the-fly evaluation, an `SExpression` Resource subtype is provided. Instances of this type contain a string that is used to evaluate an s-expression, which are effectively any arbitrary Racket expression from string or integer literals, to procedures, to syntax objects.

SExpression resources that refer to identifiers not bound in lambda arguments or `let` clauses can be provided with its `bindings` parameter, which takes a set of symbols and binds values to them.

# Building From Source


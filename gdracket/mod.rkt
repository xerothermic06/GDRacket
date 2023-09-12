#lang racket

(require racket/vector)

(provide
    vector-map-break
    read-multi
    eval-handle
    eval-source
    attach-modules)


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


(define read-multi
  (lambda (in)
    (letrec ([read-rest (lambda (in vec)
                          (let ([valu (read in)])
                            (if (eq? valu eof)
                                vec
                                (read-rest in (vector-append vec `#(,valu) )))))])
      (read-rest in #() ))))


(define eval-handle
  (lambda (sexpr name-space)
      (with-handlers ([void (lambda (exn)
                      (displayln (continuation-mark-set->context (exn-continuation-marks exn)))
                      (cons #f exn))])
          (cons #t (eval sexpr name-space)))))


(define eval-source
  (lambda (source-str module-name name-space)
    (parameterize
      ([read-accept-lang #t]
        [read-accept-reader #t]
        [compile-enforce-module-constants #f]
        [current-module-declare-name (make-resolved-module-path module-name)])
      (let ([exprs-vec         (read-multi (open-input-string source-str))]
            [break-cond        (lambda (result-vec) (not (car result-vec)))]
            [eval-handle-in-ns (lambda (sexpr) (eval-handle sexpr name-space))])
        (vector-map-break exprs-vec eval-handle-in-ns break-cond)))))


(define attach-modules
  (lambda (src-ns dest-ns module-list)
    (map
      (lambda (modul)
        (namespace-attach-module src-ns modul dest-ns))
      module-list)))

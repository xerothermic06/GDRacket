#lang racket

(require (for-syntax syntax/parse))

(provide godot-name->racket-name)
(define (godot-name->racket-name name-str)
  (string-replace "_" "-" name-str))

(provide racket-name->godot-name)
(define (racket-name->godot-name name-str)
  (string-replace "-" "_" name-str))

(provide log/info)
(define-syntax (log/info stx)
  (syntax-parse stx
    [(_ msg:expr ...)
     (begin
       (define srcloc (format "~a:~a" (syntax-source stx) (syntax-line stx)))
       (define msg-exprs (syntax->datum #'(msg ...)))
       (define message-join-expr `(string-join (for/list ([message (list ,@msg-exprs)])
                                               (format "~a" message)) " ") )
       (datum->syntax stx
                      `(displayln
                        (format "I ~a ~a" ,srcloc ,message-join-expr))))]))

;(log/info "hello" "goodbye")
;(log/info (format "~a world" "goodbye"))

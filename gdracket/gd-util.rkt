#lang racket

(provide log/info)
(define-syntax (log/info stx)
  (syntax-case stx ()
    [(log/info msg)
     (begin
       (define srcloc (format "~a:~a" (syntax-source stx) (syntax-line stx)))
       (define msg-str (format "I ~a ~a" srcloc (syntax->datum #'msg)))
       #`(displayln #,msg-str))]))

(log/info "hello")
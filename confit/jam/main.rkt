#lang br/quicklang
(require "parse.rkt")

(module+ reader
  (provide read-syntax))

(define (read-syntax path port)
  (define parse-tree (parse port))
  #`(module jam-mod jam/expander
       #,parse-tree))

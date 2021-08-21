#lang racket

(require "parse.rkt")

(module+ reader
  (provide (rename-out [jam/read read]
                       [jam/read-syntax read-syntax])))

(define (jam/read in) 
  (define parsed (jam/parse in))
  #`(module confit-mod confit/expander
       #,parsed))

(define (jam/read-syntax source-name in) (jam/read in))


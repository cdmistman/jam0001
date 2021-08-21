#lang racket

(require "parse.rkt")

(module+ reader
  (provide (rename-out [jam/read read]
                       [jam/read-syntax read-syntax])))

(define (jam/read in) 
  (define parsed (jam/parse in))
  parsed)

(define (jam/read-syntax source-name in) (jam/read in))

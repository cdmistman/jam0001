#lang racket

(require
  data/applicative
  data/monad
  megaparsack
  megaparsack/text)

(struct confit-number (is-neg? digits))

(define confit-ident/p
  (many+/p (or/p
            ; i think these are good enough?
            letter/p
            digit/p
            (char/p #\')
            (char/p #\-)
            (char/p #\/))))

(define confit-number/p
  (do 
      [neg <- (try/p (char/p #\-))]
    [digits <- (many+/p digit/p)]
    (pure neg digits)))

(define (s-expr/p expr/p)
  (do 
      ; (many)
      (string/p "(")
    [expr <- expr/p]
    (string/p ")")
    (pure expr)))

(struct confit-expr-lit (ty val))
(define confit-expr-lit-number/p
  (do
      [number <- confit-number/p]
    (pure ('Number number))))

(define confit-expr-lit/p
  (or/p
   confit-expr-lit-number/p))

(struct confit-call (name params))
(define confit-call/p
  (s-expr/p
   (do
       [name <- confit-ident/p]
     [params <- (many/p (do many+/p (pure confit-ident/p)))]
     (pure (confit-call name params)))))

(struct confit-expr (kind expr))
(define confit-expr/p
  (or/p
   confit-expr-lit/p
   confit-ident/p
   confit-call/p))

; if ty is #f, that means no type was provided
(struct confit-define (ty bind expr))

(define confit-define-untyped/p
  (do
      (string/p "define")
    [name-or-binding <- (or/p confit-ident/p confit-call/p)]
    [expr <- confit-expr/p]
    (pure (confit-define #f name-or-binding expr))))

(define confit-define/p 
  (s-expr/p
   (do (or/p
        confit-define-untyped/p))))

(define confit/p
  (do (or/p
       confit-define/p
       confit-expr/p)))

#lang racket

(require
  data/applicative
  data/monad
  megaparsack
  megaparsack/text)

(define confit-ident/p
  (do [start <- letter/p]
    [rest <- (many/p (or/p
                      ; i think these are good enough?
                      letter/p
                      digit/p
                      (char/p #\')
                      (char/p #\-)
                      (char/p #\/)))]
    (pure (list->string (cons start rest)))))

(define (keyword/p word) ; prevents e.g. "definex" to match "define"
  (do [kw <- (guard/p (many+/p (char-between/p #\a #\z)) (λ (chars) (equal? (list->string chars) word)))]
    (pure (list->string kw))))

(define whitespace?/p
  (many/p space/p))

(define (?/p p)
  (many/p p #:max 1))

(struct confit-number (is-neg? digits))
(define confit-number/p
  (do 
      [neg <- (?/p (char/p #\-))]
    [digits <- (many+/p digit/p)]
    (pure (confit-number neg digits))))

(define (s-expr/p expr/p)
  (do 
      (string/p "(")
    [expr <- expr/p]
    (string/p ")")
    (pure expr)))

(struct confit-expr-lit (ty val))
(define confit-expr-lit-number/p
  (do
      [number <- confit-number/p]
    (pure '(Number number))))

(define confit-expr-lit/p
  (or/p
   confit-expr-lit-number/p))

(struct confit-call (name params))
(define confit-call/p
  (s-expr/p
   (do
       [name <- confit-ident/p]
     whitespace?/p
     [params <- (many/p confit-ident/p #:sep space/p)]
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
      (keyword/p "define")
    whitespace?/p
    [name-or-binding <- (or/p confit-ident/p confit-call/p)]
    whitespace?/p
    [expr <- confit-expr/p]
    (pure (confit-define #f name-or-binding expr))))

(define confit-define/p 
  (s-expr/p
   (do (or/p
        confit-define-untyped/p))))

(define confit/p
  (do (or/p
       (try/p confit-define/p)
       confit-expr/p)))

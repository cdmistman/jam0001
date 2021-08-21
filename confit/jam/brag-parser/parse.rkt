#lang racket

(require brag/support
         (rename-in "grammar.rkt" [parse brag-parse]))

(provide parse)

(define (parse src)
  (let [[ast (syntax->datum (brag-parse (tokenise (port->string src))))]]
    ;(write ast)
    ast))

(define (tokenise str [tokens '[]])
  (if (equal? str "")
      (reverse tokens)
      (let [[next-token
             (regexp-case
              str
              [#rx"^[\n\t ]+" (token 'WS (first Match))]
              [#rx"^(\"\"|\"([^\"]|[\\]\")*[^\\]\")" (token 'STRING (first Match))]
              [#rx"^-?[0-9]+(\\.[0-9]+)?" (token 'NUMBER (first Match))]
              [#rx"^[a-zA-Z!£$%^&*_+=:.<>?/#~@-][a-zA-Z0-9!£$%^&*_+=:.<>?/#~@-]*" (token 'SYMBOL (first Match))]
              [#rx"^'" (token 'QUOTE (first Match))]
              [#rx"^`" (token 'QUASIQUOTE (first Match))]
              [#rx"^," (token 'UNQUOTE (first Match))]
              [#rx"^,@" (token 'UNQUOTE-SPLICE (first Match))]
              [#rx"^;;([^;]|[^;];[^;])*;;" (token 'BLOCK-COMMENT (first Match))]
              [#rx"^;[^\n]*(\n|$)" (token 'LINE-COMMENT (first Match))]
              [#rx"^\\(" (token 'OPEN (first Match))]
              [#rx"^\\)" (token 'CLOSE (first Match))]
              [else (error (format "could not continue tokenising from ~a" str))])]]
        (tokenise (substring str (string-length (token-struct-val next-token)))
                  (if (equal? (token-struct-type next-token) 'WS)
                      tokens
                      (cons next-token tokens))))))

(define-syntax (regexp-case stx)
  (let* [[dtm (syntax->datum stx)]
         [str (cadr dtm)]
         [cases (cddr dtm)]]
    (datum->syntax
     stx
     `(cond ,@(map (λ (case)
                     (if (equal? (car case) 'else)
                         case
                         `[(regexp-match? ,(car case) ,str)
                           (let [[Match (regexp-match ,(car case) ,str)]]
                             ,@(cdr case))])) cases)))))
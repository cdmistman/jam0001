#lang br/quicklang
(provide (rename-out [module-begin #%module-begin]))
(require (for-syntax syntax/parse syntax/free-vars))

(define-macro (module-begin (LINE ...))
  (with-pattern ()
    #'(#%module-begin
       (LINE ...)
       )))

(define-syntax (comment->lambda comment)
  (syntax-parse comment
    [(_ _ expr) (define expanded-expr (local-expand #'expr 'expression '()))
     (syntax-parse expanded-expr
       [exprs (define args-syntax (datum->syntax (free-vars #'exprs)))
        #'(lambda #'args-syntax #'exprs)])]))

(define-syntax comment
  (syntax-rules ()
    ((comment is-comment? lam-body)
     (let ([lam (comment->lambda (datum->syntax (quote lam-body)))])
       `('comment ,is-comment? lam)))))
(provide comment)

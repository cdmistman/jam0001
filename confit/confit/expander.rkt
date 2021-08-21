#lang br/quicklang
(provide (rename-out [module-begin #%module-begin]))

(define-macro (module-begin (LINE ...))
  (with-pattern ()
    #'(#%module-begin
       (LINE ...)
       )))

(define (comment flag contents)
  (println `(comment ,flag ,contents)))
(provide comment)

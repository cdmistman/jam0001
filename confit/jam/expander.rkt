#lang br/quicklang
(provide (rename-out [module-begin #%module-begin]))

(define-macro (module-begin (LINE ...))
  (with-pattern ()
    #'(#%module-begin
       (module configure-runtime racket
       LINE ...
       ))))

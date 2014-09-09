(define-library (np lang impl macros structure-meta-vars)
  ;;;
  ;;; Structural analysis of meta-variable specifier lists
  ;;;
  (export $can-be:standalone-meta-var?
          $must-be:standalone-meta-var

          $can-be:extension-meta-var?
          $must-be:extension-meta-var

          $can-be:meta-var-addition?
          $can-be:meta-var-removal?)

  (import (scheme base)
          (sr ck)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates)
          (np lang impl macros utils))

  (begin

    (define-syntax $can-be:standalone-meta-var?
      (syntax-rules (quote)
        ((_ s 'expr) ($ s ($not-vector-or-list? 'expr))) ) )

    (define-syntax $must-be:standalone-meta-var
      (syntax-rules (quote)
        ((_ s 'lang 'clause '(x ...))
         (syntax-error "Meta-var name cannot be a list" lang clause (x ...)))

        ((_ s 'lang 'clause '#(x ...))
         (syntax-error "Meta-var name cannot be a vector" lang clause #(x ...)))

        ((_ s _ _ 'var) ($ s 'var)) ) )

    (define-syntax $can-be:extension-meta-var?
      (syntax-rules (quote + -)
        ((_ s '(+ vars ...))
         ($ s ($every? '$can-be:standalone-meta-var? '(vars ...))))
        ((_ s '(- vars ...))
         ($ s ($every? '$can-be:standalone-meta-var? '(vars ...))))
        ((_ s _) ($ s '#f)) ) )

    (define-syntax $must-be:extension-meta-var
      (syntax-rules (quote + -)
        ((_ s 'lang 'clause '(+))
         (syntax-error "List of added meta-vars cannot be empty" lang clause))

        ((_ s 'lang 'clause '(-))
         (syntax-error "List of removed meta-vars cannot be empty" lang clause))

        ((_ s 'lang 'clause '(+ vars ...))
         ($ s ($list '+
                ($map '($must-be:standalone-meta-var 'lang 'clause)
                      '(vars ...) ) )))

        ((_ s 'lang 'clause '(- vars ...))
         ($ s ($list '-
                ($map '($must-be:standalone-meta-var 'lang 'clause)
                      '(vars ...) ) )))

        ((_ s 'lang 'clause 'invalid-expr)
         (syntax-error "Invalid meta-var description" lang clause invalid-expr)) ) )

    (define-syntax $can-be:meta-var-addition?
      (syntax-rules (quote +)
        ((_ s '(+ vars ...))
         ($ s ($every? '$can-be:standalone-meta-var? '(vars ...))))
        ((_ s _) ($ s '#f)) ) )

    (define-syntax $can-be:meta-var-removal?
      (syntax-rules (quote -)
        ((_ s '(- vars ...))
         ($ s ($every? '$can-be:standalone-meta-var? '(vars ...))))
        ((_ s _) ($ s '#f)) ) )

) )

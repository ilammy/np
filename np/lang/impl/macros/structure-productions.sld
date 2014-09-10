(define-library (np lang impl macros structure-productions)
  ;;;
  ;;; Structural analysis of nonterminal productions (standalone and extension)
  ;;;
  (export $can-be:standalone-production?
          $must-be:standalone-production)

  (import (scheme base)
          (sr ck)
          (sr ck maps)
          (sr ck predicates))

  (begin

    ;;;
    ;;; Standalone form
    ;;;

    (define-syntax $can-be:standalone-production?
      (syntax-rules (quote)
        ((_ s '#(x ...)) ($ s '#f))
        ((_ s '(x ...))
         ($ s ($every? '$can-be:standalone-production? '(x ...))))
        ((_ s 'atom) ($ s '#t)) ) )

    (define-syntax $must-be:standalone-production
      (syntax-rules (quote)
        ((_ s 'lang 'clause '#(x ...))
         (syntax-error "Production cannot be a vector" lang clause #(x ...)))

        ((_ s 'lang 'clause '(x ...))
         ($ s ($map '($must-be:standalone-production* 'lang 'clause '(x ...)) '(x ...))))

        ((_ s _ _ 'atom) ($ s 'atom)) ) )

    (define-syntax $must-be:standalone-production*
      (syntax-rules (quote)
        ((_ s 'lang 'clause 'production '#(x ...))
         (syntax-error "Production cannot contain a vector" lang clause production #(x ...)))

        ((_ s 'lang 'clause 'production '(x ...))
         ($ s ($map '($must-be:standalone-production* 'lang 'clause 'production) '(x ...))))

        ((_ s _ _ _ 'atom) ($ s 'atom)) ) )

) )

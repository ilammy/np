(define-library (np lang impl macros structure-productions)
  ;;;
  ;;; Structural analysis of nonterminal productions (standalone and extension)
  ;;;
  (export %verify:standalone-production
          $can-be:standalone-production?
          $must-be:standalone-production

          $can-be:production-addition?
          $can-be:production-removal?)

  (import (scheme base)
          (sr ck)
          (sr ck maps)
          (sr ck predicates))

  (begin

    ;;;
    ;;; Standalone form
    ;;;

    (define-syntax %verify:standalone-production
      (syntax-rules (quote)
        ((_ s '(k t) '#(x ...))
         ($ k '("Incorrect production syntax: vector patterns are not allowed" (#(x ...) . t))))

        ((_ s '(k t) 'otherwise)
         ($ s (%verify:production* '(k (otherwise . t)) 'otherwise))) ) )

    (define-syntax %verify:standalone-production*
      (syntax-rules (quote)
        ((_ s '(k t) '#(x ...))
         ($ k '("Incorrect production syntax: vector patterns are not allowed" (#(x ...) . t))))

        ((_ s '(k t) '(a . d)) ($ s ($and '(%verify:production* '(k t) 'a)
                                          '(%verify:production* '(k t) 'd) )))

        ((_ s '(k t) _) ($ s '#t)) ) )

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

    ;;;
    ;;; Extension form
    ;;;

    (define-syntax $can-be:production-addition?
      (syntax-rules (quote +)
        ((_ s '(+ clauses ...))
         ($ s ($every? '$can-be:standalone-production? '(clauses ...)))) ) )

    (define-syntax $can-be:production-removal?
      (syntax-rules (quote -)
        ((_ s '(- clauses ...))
         ($ s ($every? '$can-be:standalone-production? '(clauses ...)))) ) )


) )

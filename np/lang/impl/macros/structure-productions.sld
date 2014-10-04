(define-library (np lang impl macros structure-productions)
  ;;;
  ;;; Structural analysis of nonterminal productions (standalone and extension)
  ;;;
  (export %verify:standalone-production
          $can-be:standalone-production?
          $must-be:standalone-production

          $can-be:extension-production?

          $can-be:production-addition?
          $can-be:production-removal?

          %verify:production-modification

          $squash-extension-productions)

  (import (scheme base)
          (sr ck)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates)
          (np lang impl macros utils))

  (begin

    ;;;
    ;;; Standalone form
    ;;;

    (define-verifier %verify:standalone-production
      (syntax-rules (quote)
        ((_ s '(k t) 'term '#(x ...))
         ($ k '("Invalid syntax of the production: vector patterns are not allowed" (term . t))))

        ((_ s '(k t) 'term _)
         ($ s (%verify:standalone-production* '(k (term . t)) 'term))) ) )

    (define-verifier %verify:standalone-production*
      (syntax-rules (quote)
        ((_ s '(k t) 'term '#(x ...))
         ($ k '("Invalid syntax of the production: vector patterns are not allowed" (term . t))))

        ((_ s '(k t) 'term '(a . d)) ($ s ($and '(%verify:standalone-production* '(k t) 'a)
                                                '(%verify:standalone-production* '(k t) 'd) )))

        ((_ s '(k t) 'term _) ($ s '#t)) ) )

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

    (define-syntax $can-be:extension-production?
      (syntax-rules (quote + -)
        ((_ s '(+ . rest)) ($ s '#t))
        ((_ s '(- . rest)) ($ s '#t))
        ((_ s  _)          ($ s '#f)) ) )

    (define-syntax $can-be:production-addition?
      (syntax-rules (quote +)
        ((_ s '(+ . production-list)) ($ s '#t))
        ((_ s  _)                     ($ s '#f)) ) )

    (define-syntax $can-be:production-removal?
      (syntax-rules (quote -)
        ((_ s '(- . production-list)) ($ s '#t))
        ((_ s  _)                     ($ s '#f)) ) )

    (define-verifier %verify:production-modification
      (syntax-rules (quote + -)
        ((_ s '(k t) 'term '(+ . production-list))
         ($ s ($and '(%verify:production-addition-list '(k (term . t)) 'production-list)
                    '($every? '(%verify:standalone-production '(k t)) 'production-list) )))

        ((_ s '(k t) 'term '(- . production-list))
         ($ s ($and '(%verify:production-removal-list '(k (term . t)) 'production-list)
                    '($every? '(%verify:standalone-production '(k t)) 'production-list) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the production modification" (term . t)))) ) )

    (define-verifier/proper-nonempty-list:report-dot-only %verify:production-addition-list
      ("At least one production should be specified for addition"
       "Unexpected dotted list in production modification"
       "Expected a list of productions") )

    (define-verifier/proper-nonempty-list:report-dot-only %verify:production-removal-list
      ("At least one production should be specified for removal"
       "Unexpected dotted list in production modification"
       "Expected a list of productions") )

    ;;;
    ;;; Squashers
    ;;;

    (define-syntax $squash-extension-productions
      (syntax-rules (quote)
        ((_ s 'prods) ($ s ($concatenate ($map '$cdr 'prods)))) ) )

) )

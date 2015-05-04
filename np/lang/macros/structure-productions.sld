(define-library (np lang macros structure-productions)
  ;;;
  ;;; Structural analysis of nonterminal productions (standalone and extension)
  ;;;
  (export %verify:standalone-production
          %verify:production-modification

          $can-be:production-addition?
          $can-be:production-removal?

          $squash-extension-productions)

  (import (scheme base)
          (sr ck)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates)
          (np lang macros verify-utils))

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

    ;;;
    ;;; Extension form
    ;;;

    (define-syntax $can-be:production-addition?
      (syntax-rules (quote +)
        ((_ s '(+ productions ...)) ($ s '#t))
        ((_ s  _)                   ($ s '#f)) ) )

    (define-syntax $can-be:production-removal?
      (syntax-rules (quote -)
        ((_ s '(- productions ...)) ($ s '#t))
        ((_ s  _)                   ($ s '#f)) ) )

    (define-verifier %verify:production-modification
      (syntax-rules (quote + -)
        ((_ s '(k t) 'term '(+ productions ...))
         ($ s ($and '(%verify:production-addition-list '(k (term . t)) '(productions ...))
                    '($every? '(%verify:standalone-production '(k t)) '(productions ...)) )))

        ((_ s '(k t) 'term '(- productions ...))
         ($ s ($and '(%verify:production-removal-list '(k (term . t)) '(productions ...))
                    '($every? '(%verify:standalone-production '(k t)) '(productions ...)) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the production modification" (term . t)))) ) )

    (define-verifier/nonempty-list %verify:production-addition-list
      ("At least one production should be specified for addition") )

    (define-verifier/nonempty-list %verify:production-removal-list
      ("At least one production should be specified for removal") )

    ;;;
    ;;; Squashers
    ;;;

    (define-syntax $squash-extension-productions
      (syntax-rules (quote)
        ((_ s 'productions) ($ s ($concatenate ($map '$cdr 'productions)))) ) )

) )

(define-library (np lang impl macros structure-meta-vars)
  ;;;
  ;;; Structural analysis of meta-variable specifier lists
  ;;;
  (export %verify:meta-var-name
          %verify:meta-var-modification

          $can-be:meta-var-addition?
          $can-be:meta-var-removal?

          $squash-extension-meta-variables)

  (import (scheme base)
          (sr ck)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates)
          (np lang impl macros verify-utils))

  (begin

    ;;;
    ;;; Standalone meta-vars
    ;;;

    (define-verifier/atom %verify:meta-var-name
      ("Name of the meta-variable must be a symbol") )

    ;;;
    ;;; Extension meta-vars
    ;;;

    (define-syntax $can-be:meta-var-addition?
      (syntax-rules (quote +)
        ((_ s '(+ . meta-var-name-list)) ($ s '#t))
        ((_ s  _)                        ($ s '#f)) ) )

    (define-syntax $can-be:meta-var-removal?
      (syntax-rules (quote -)
        ((_ s '(- . meta-var-name-list)) ($ s '#t))
        ((_ s  _)                        ($ s '#f)) ) )

    (define-verifier %verify:meta-var-modification
      (syntax-rules (quote + -)
        ((_ s '(k t) 'term '(+ . meta-var-name-list))
         ($ s ($and '(%verify:meta-var-addition-list '(k (term . t)) 'meta-var-name-list)
                    '($every? '(%verify:meta-var-name '(k t)) 'meta-var-name-list) )))

        ((_ s '(k t) 'term '(- . meta-var-name-list))
         ($ s ($and '(%verify:meta-var-removal-list '(k (term . t)) 'meta-var-name-list)
                    '($every? '(%verify:meta-var-name '(k t)) 'meta-var-name-list) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the meta-variable modification" (term . t)))) ) )

    (define-verifier/proper-nonempty-list:report-dot-only %verify:meta-var-addition-list
      ("At least one meta-variable should be specified for addition"
       "Unexpected dotted list in meta-variable modification"
       "Expected a list of meta-variables") )

    (define-verifier/proper-nonempty-list:report-dot-only %verify:meta-var-removal-list
      ("At least one meta-variable should be specified for removal"
       "Unexpected dotted list in meta-variable modification"
       "Expected a list of meta-variables") )

    ;;;
    ;;; Squashers
    ;;;

    (define-syntax $squash-extension-meta-variables
      (syntax-rules (quote)
        ((_ s 'meta-vars) ($ s ($concatenate ($map '$cdr 'meta-vars)))) ) )

) )

(define-library (np lang impl macros structure-meta-vars)
  ;;;
  ;;; Structural analysis of meta-variable specifier lists
  ;;;
  (export %verify:meta-var-name

          $can-be:extension-meta-var?

          $is-a:meta-var-addition?
          $is-a:meta-var-removal?

          $squash-extension-meta-variables)

  (import (scheme base)
          (sr ck)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates))

  (begin

    ;;;
    ;;; Standalone meta-vars
    ;;;

    (define-syntax %verify:meta-var-name
      (syntax-rules (quote)
        ((_ s '(k t) '())       ($ k '("Meta-variable name must be a symbol" (()       . t))))
        ((_ s '(k t) '(a . d))  ($ k '("Meta-variable name must be a symbol" ((a . d)  . t))))
        ((_ s '(k t) '#(x ...)) ($ k '("Meta-variable name must be a symbol" (#(x ...) . t))))
        ((_ s '(k t)  _)        ($ s '#t)) ) )

    ;;;
    ;;; Extension meta-vars clauses
    ;;;

    (define-syntax $can-be:extension-meta-var?
      (syntax-rules (quote + -)
        ((_ s '(+ . _)) ($ s '#t))
        ((_ s '(- . _)) ($ s '#t))
        ((_ s  _)       ($ s '#f)) ) )

    (define-syntax $is-a:meta-var-addition?
      (syntax-rules (quote +)
        ((_ s '(+ vars ...)) ($ s ($every? '$is-a:meta-var-name? '(vars ...))))
        ((_ s  _)            ($ s '#f)) ) )

    (define-syntax $is-a:meta-var-removal?
      (syntax-rules (quote -)
        ((_ s '(- vars ...)) ($ s ($every? '$is-a:meta-var-name? '(vars ...))))
        ((_ s  _)            ($ s '#f)) ) )

    (define-syntax $is-a:meta-var-name?
      (syntax-rules (quote)
        ((_ s '())       ($ s '#f))
        ((_ s '(a . d))  ($ s '#f))
        ((_ s '#(x ...)) ($ s '#f))
        ((_ s  _)        ($ s '#t)) ) )

    (define-syntax $squash-extension-meta-variables
      (syntax-rules (quote)
        ((_ s 'vars) ($ s ($concatenate ($map '$cdr 'vars)))) ) )
) )

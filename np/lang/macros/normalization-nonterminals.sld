(define-library (np lang macros normalization-nonterminals)
  ;;;
  ;;; Normalizing partitioned nonterminal definition clauses
  ;;;
  (export $normalize-standalone-nonterminal-definition
          $normalize-extension-nonterminal-addition
          $normalize-extension-nonterminal-removal
          $normalize-extension-nonterminal-modification)

  (import (scheme base)
          (sr ck))

  (begin

    (define-syntax $normalize-standalone-nonterminal-definition
      (syntax-rules (quote)
        ((_ s '(name predicate-name (meta-vars ...) productions ...))
         ($ s '(name predicate-name (meta-vars ...) (productions ...))))
        ((_ s '(name                (meta-vars ...) productions ...))
         ($ s '(name #f             (meta-vars ...) (productions ...)))) ) )

    (define-syntax $normalize-extension-nonterminal-addition
      (syntax-rules (quote)
        ((_ s '(name predicate-name (meta-vars ...) productions ...))
         ($ s '(name predicate-name (meta-vars ...) (productions ...))))
        ((_ s '(name                (meta-vars ...) productions ...))
         ($ s '(name #f             (meta-vars ...) (productions ...)))) ) )

    (define-syntax $normalize-extension-nonterminal-removal
      (syntax-rules (quote)
        ((_ s '(name predicate-name (meta-vars ...) productions ...)) ($ s 'name))
        ((_ s '(name                (meta-vars ...) productions ...)) ($ s 'name))
        ((_ s 'name)                                                  ($ s 'name)) ) )

    (define-syntax $normalize-extension-nonterminal-modification
      (syntax-rules (quote)
        ((_ s '(name predicate-name (meta-var-additions meta-var-removals) (production-additions production-removals)))
         ($ s '(name predicate-name  meta-var-additions meta-var-removals   production-additions production-removals)))
        ((_ s '(name                (meta-var-additions meta-var-removals) (production-additions production-removals)))
         ($ s '(name #f              meta-var-additions meta-var-removals   production-additions production-removals))) ) )

) )

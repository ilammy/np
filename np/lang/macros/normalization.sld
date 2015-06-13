(define-library (np lang macros normalization)
  ;;;
  ;;; Normalizing partitioned definition clauses
  ;;;
  (export $normalize-standalone-terminal-definition
          $normalize-extension-terminal-addition
          $normalize-extension-terminal-removal
          $normalize-extension-terminal-modification

          $normalize-standalone-nonterminal-definition
          $normalize-extension-nonterminal-addition
          $normalize-extension-nonterminal-removal
          $normalize-extension-nonterminal-modification)

  (import (scheme base)
          (sr ck))

  (begin

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;
    ;;; Terminal clauses
    ;;;

    (define-syntax $normalize-standalone-terminal-definition
      (syntax-rules (quote)
        ((_ s '(name predicate meta-var-list))
         ($ s '(name predicate meta-var-list))) ) )

    (define-syntax $normalize-extension-terminal-addition
      (syntax-rules (quote)
        ((_ s '(name predicate meta-var-list))
         ($ s '(name predicate meta-var-list))) ) )

    (define-syntax $normalize-extension-terminal-removal
      (syntax-rules (quote)
        ((_ s '(name predicate meta-var-list)) ($ s 'name))
        ((_ s 'name)                           ($ s 'name)) ) )

    (define-syntax $normalize-extension-terminal-modification
      (syntax-rules (quote)
        ((_ s '(name (meta-var-additions meta-var-removals)))
         ($ s '(name meta-var-additions meta-var-removals))) ) )


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;
    ;;; Nonterminal clauses
    ;;;

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

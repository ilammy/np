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
        ((_ s '(name #(predicate-name) meta-var-list . production-list))
         ($ s '(name predicate-name meta-var-list production-list)))
        ((_ s '(name                   meta-var-list . production-list))
         ($ s '(name #f meta-var-list production-list))) ) )

    (define-syntax $normalize-extension-nonterminal-addition
      (syntax-rules (quote)
        ((_ s '(name #(predicate-name) meta-var-list . production-list))
         ($ s '(name predicate-name meta-var-list production-list)))
        ((_ s '(name                   meta-var-list . production-list))
         ($ s '(name #f meta-var-list production-list))) ) )

    (define-syntax $normalize-extension-nonterminal-removal
      (syntax-rules (quote)
        ((_ s '(name #(predicate-name) meta-var-list . production-list)) ($ s 'name))
        ((_ s '(name                   meta-var-list . production-list)) ($ s 'name))
        ((_ s 'name)                                                     ($ s 'name)) ) )

    (define-syntax $normalize-extension-nonterminal-modification
      (syntax-rules (quote)
        ((_ s '(name (meta-var-additions meta-var-removals)
                     (production-additions production-removals) ))
         ($ s '(name meta-var-additions meta-var-removals
                     production-additions production-removals ))) ) )

) )

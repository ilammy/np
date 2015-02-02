(define-library (np lang macros normalization-terminals)
  ;;;
  ;;; Normalizing partitioned terminal definition clauses
  ;;;
  (export $normalize-standalone-terminal-definition
          $normalize-extension-terminal-addition
          $normalize-extension-terminal-removal
          $normalize-extension-terminal-modification)

  (import (scheme base)
          (sr ck))

  (begin

    (define-syntax $normalize-standalone-terminal-definition
      (syntax-rules (quote)
        ((_ s '(name predicate meta-var-list))
         ($ s '(name predicate meta-var-list)))
        ((_ s '(predicate-name meta-var-list))
         ($ s '(predicate-name predicate-name meta-var-list))) ) )

    (define-syntax $normalize-extension-terminal-addition
      (syntax-rules (quote)
        ((_ s '(name predicate meta-var-list))
         ($ s '(name predicate meta-var-list)))
        ((_ s '(predicate-name meta-var-list))
         ($ s '(predicate-name predicate-name meta-var-list))) ) )

    (define-syntax $normalize-extension-terminal-removal
      (syntax-rules (quote)
        ((_ s '(name predicate meta-var-list)) ($ s 'name))
        ((_ s '(predicate-name meta-var-list)) ($ s 'predicate-name))
        ((_ s 'name)                           ($ s 'name)) ) )

    (define-syntax $normalize-extension-terminal-modification
      (syntax-rules (quote)
        ((_ s '(name (meta-var-additions meta-var-removals)))
         ($ s '(name meta-var-additions meta-var-removals))) ) )

) )

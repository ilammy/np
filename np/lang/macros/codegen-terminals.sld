(define-library (np lang macros codegen-terminals)
  ;;;
  ;;; Codegen for normalized terminal definition clauses
  ;;;
  (export $generate-standalone-terminal-definitions
          $generate-extension-terminal-additions
          $generate-extension-terminal-removals
          $generate-extension-terminal-modifications)

  (import (scheme base)
          (np lang descriptions types)
          (sr ck)
          (sr ck kernel)
          (sr ck lists)
          (sr ck maps))

  (begin

    (define-syntax $generate-standalone-terminal-definitions
      (syntax-rules (quote)
        ((_ s 'definitions)
         ($ s ($cons 'list
           ($map '$make-terminal-definition 'definitions) ))) ) )

    (define-syntax $generate-extension-terminal-additions
      (syntax-rules (quote)
        ((_ s 'definitions)
         ($ s ($cons 'list
           ($map '$make-terminal-definition 'definitions) ))) ) )

    (define-syntax $make-terminal-definition
      (syntax-rules (quote)
        ((_ s '(name predicate meta-variables))
         ($ s '(make-terminal-definition 'name predicate 'meta-variables))) ) )

    (define-syntax $generate-extension-terminal-removals
      (syntax-rules (quote)
        ((_ s 'terminal-names)
         ($ s ($cons 'list ($map '$quote 'terminal-names)))) ) )

    (define-syntax $generate-extension-terminal-modifications
      (syntax-rules (quote)
        ((_ s 'modifications)
         ($ s ($cons 'list
           ($map '$make-terminal-modification 'modifications) ))) ) )

    (define-syntax $make-terminal-modification
      (syntax-rules (quote)
        ((_ s '(name added-meta-variables removed-meta-variables))
         ($ s '(make-terminal-modification
           'name 'added-meta-variables 'removed-meta-variables ))) ) )

) )

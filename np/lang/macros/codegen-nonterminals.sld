(define-library (np lang macros codegen-nonterminals)
  ;;;
  ;;; Codegen for normalized nonterminal definition clauses
  ;;;
  (export $generate-standalone-nonterminal-definitions
          $generate-standalone-nonterminal-predicate-definitions
          $generate-extension-nonterminal-additions
          $generate-extension-nonterminal-removals
          $generate-extension-nonterminal-modifications
          $generate-extension-nonterminal-predicate-definitions)

  (import (scheme base)
          (np lang descriptions language)
          (np lang descriptions types)
          (sr ck)
          (sr ck kernel)
          (sr ck lists)
          (sr ck maps))

  (begin

    (define-syntax $generate-standalone-nonterminal-definitions
      (syntax-rules (quote)
        ((_ s 'definitions)
         ($ s ($cons 'list
           ($map '$make-nonterminal-definition 'definitions) ))) ) )

    (define-syntax $generate-extension-nonterminal-additions
      (syntax-rules (quote)
        ((_ s 'definitions)
         ($ s ($cons 'list
           ($map '$make-nonterminal-definition 'definitions) ))) ) )

    (define-syntax $make-nonterminal-definition
      (syntax-rules (quote)
        ((_ s '(name predicate meta-variables production-descriptions))
         ($ s '(make-nonterminal-definition
           'name 'meta-variables 'production-descriptions ))) ) )

    (define-syntax $generate-extension-nonterminal-removals
      (syntax-rules (quote)
        ((_ s 'nonterminal-names)
         ($ s ($cons 'list ($map '$quote 'nonterminal-names)))) ) )

    (define-syntax $generate-extension-nonterminal-modifications
      (syntax-rules (quote)
        ((_ s 'modifications)
         ($ s ($cons 'list
           ($map '$make-nonterminal-modification 'modifications) ))) ) )

    (define-syntax $make-nonterminal-modification
      (syntax-rules (quote)
        ((_ s '(name added-meta-variables removed-meta-variables
                added-production-descriptions removed-production-descriptions ))
         ($ s '(make-nonterminal-modification
           'name 'added-meta-variables 'added-production-descriptions
           'removed-meta-variables 'removed-production-descriptions ))) ) )

    (define-syntax $generate-standalone-nonterminal-predicate-definitions
      (syntax-rules (quote)
        ((_ s 'lang 'definitions)
         ($ s ($cons 'begin
           ($map '($define-nonterminal-predicate 'lang) 'definitions) ))) ) )

    (define-syntax $generate-extension-nonterminal-predicate-definitions
      (syntax-rules (quote)
        ((_ s 'lang 'definitions)
         ($ s ($cons 'begin
           ($map '($define-nonterminal-predicate 'lang) 'definitions) ))) ) )

    (define-syntax $define-nonterminal-predicate
      (syntax-rules (quote)
        ((_ s 'lang '(name     #f    meta-variables production-descriptions))
         ($ s '#t))
        ((_ s 'lang '(name predicate meta-variables production-descriptions))
         ($ s '(define predicate (language-nonterminal-predicate lang 'name)))) ) )

) )

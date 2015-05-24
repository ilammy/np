(define-library (np lang macros codegen)
  ;;;
  ;;; Codegen for normalized clauses
  ;;;
  (export $generate-toplevel-predicate-definition
          $generate-toplevel-parser-definition
          $generate-toplevel-unparser-definition

          $generate-standalone-terminal-definitions
          $generate-extension-terminal-additions
          $generate-extension-terminal-removals
          $generate-extension-terminal-modifications

          $generate-standalone-nonterminal-definitions
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

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;
    ;;; Toplevel clauses
    ;;;

    (define-syntax $generate-toplevel-predicate-definition
      (syntax-rules (quote)
        ((_ s 'lang '#f)
         ($ s '#t))
        ((_ s 'lang '(binding))
         ($ s '(define binding (language-predicate lang)))) ) )

    (define-syntax $generate-toplevel-parser-definition
      (syntax-rules (quote)
        ((_ s 'lang '#f)
         ($ s '#t))
        ((_ s 'lang '(binding))
         ($ s '(define binding (language-parser lang)))) ) )

    (define-syntax $generate-toplevel-unparser-definition
      (syntax-rules (quote)
        ((_ s 'lang '#f)
         ($ s '#t))
        ((_ s 'lang '(binding))
         ($ s '(define binding (language-unparser lang)))) ) )


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;
    ;;; Terminal definitions
    ;;;

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


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;
    ;;; Nonterminal definitions
    ;;;

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
        ((_ s '(name predicate-name added-meta-variables removed-meta-variables
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
         ($ s '(define predicate (language-nonterminal-predicate lang 'name))))
        ((_ s 'lang '(name     #f    added-meta-vars removed-meta-vars added-productions removed-productions))
         ($ s '#t))
        ((_ s 'lang '(name predicate added-meta-vars removed-meta-vars added-productions removed-productions))
         ($ s '(define predicate (language-nonterminal-predicate lang 'name)))) ) )

) )

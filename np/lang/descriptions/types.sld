(define-library (np lang descriptions types)
  ;;;
  ;;; Types used in processing of language definitions and descriptions
  ;;;
  (export terminal-definition terminal-definition? make-terminal-definition
          terminal-name
          terminal-predicate
          terminal-meta-variables

          terminal-modification terminal-modification? make-terminal-modification
          modified-terminal-name
          modified-terminal-added-meta-variables
          modified-terminal-removed-meta-variables

          nonterminal-definition nonterminal-definition? make-nonterminal-definition
          nonterminal-name
          nonterminal-meta-variables
          nonterminal-production-definitions

          nonterminal-modification nonterminal-modification? make-nonterminal-modification
          modified-nonterminal-name
          modified-nonterminal-added-meta-variables
          modified-nonterminal-added-production-definitions
          modified-nonterminal-removed-meta-variables
          modified-nonterminal-removed-production-definitions

          lang-error lang-error?
          lang-error-kind
          lang-error-object
          lang-error-causes)

  (import (scheme base))

  (begin
    ;;;
    ;;; Terminal and nonterminal definitions as given by the user
    ;;;

    (define-record-type terminal-definition
      (make-terminal-definition name predicate meta-variables)
      terminal-definition?
      (name           terminal-name)
      (predicate      terminal-predicate)
      (meta-variables terminal-meta-variables) )

    (define-record-type terminal-modification
      (make-terminal-modification name added-meta-variables removed-meta-variables)
      terminal-modification?
      (name                   modified-terminal-name)
      (added-meta-variables   modified-terminal-added-meta-variables)
      (removed-meta-variables modified-terminal-removed-meta-variables) )

    (define-record-type nonterminal-definition
      (make-nonterminal-definition name meta-variables production-definitions)
      nonterminal-definition?
      (name                   nonterminal-name)
      (meta-variables         nonterminal-meta-variables)
      (production-definitions nonterminal-production-definitions) )

    (define-record-type nonterminal-modification
      (make-nonterminal-modification name
        added-meta-variables added-production-definitions
        removed-meta-variables removed-production-definitions )
      nonterminal-modification?
      (name                           modified-nonterminal-name)
      (added-meta-variables           modified-nonterminal-added-meta-variables)
      (added-production-definitions   modified-nonterminal-added-production-definitions)
      (removed-meta-variables         modified-nonterminal-removed-meta-variables)
      (removed-production-definitions modified-nonterminal-removed-production-definitions) )

    ;;;
    ;;; Error report object
    ;;;

    (define-record-type %lang-error
      (make-lang-error kind object causes)
      lang-error?
      (kind   lang-error-kind)
      (object lang-error-object)
      (causes lang-error-causes) )

    (define (lang-error kind object . causes)
      (make-lang-error kind object causes) )

) )

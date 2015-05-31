(define-library (np lang descriptions types)
  ;;;
  ;;; Types used in processing of language definitions and descriptions
  ;;;
  (export terminal-definition terminal-definition?
          make-terminal-definition
          with-terminal-definition
          terminal-name
          terminal-predicate
          terminal-meta-variables

          terminal-modification terminal-modification?
          make-terminal-modification
          with-terminal-modification
          modified-terminal-name
          modified-terminal-added-meta-variables
          modified-terminal-removed-meta-variables

          nonterminal-definition nonterminal-definition?
          make-nonterminal-definition
          with-nonterminal-definition
          nonterminal-name
          nonterminal-meta-variables
          nonterminal-production-definitions

          nonterminal-modification nonterminal-modification?
          make-nonterminal-modification
          with-nonterminal-modification
          modified-nonterminal-name
          modified-nonterminal-added-meta-variables
          modified-nonterminal-removed-meta-variables
          modified-nonterminal-added-production-definitions
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

    (define-syntax with-terminal-definition
      (syntax-rules ()
        ((_ terminal (name predicate meta-variables) expr ...)
         (let ((name           (terminal-name           terminal))
               (predicate      (terminal-predicate      terminal))
               (meta-variables (terminal-meta-variables terminal)))
           expr ...)) ) )

    (define-record-type terminal-modification
      (make-terminal-modification name added-meta-variables removed-meta-variables)
      terminal-modification?
      (name                   modified-terminal-name)
      (added-meta-variables   modified-terminal-added-meta-variables)
      (removed-meta-variables modified-terminal-removed-meta-variables) )

    (define-syntax with-terminal-modification
      (syntax-rules ()
        ((_ terminal (name added-meta-variables removed-meta-variables) expr ...)
         (let ((name                   (modified-terminal-name                   terminal))
               (added-meta-variables   (modified-terminal-added-meta-variables   terminal))
               (removed-meta-variables (modified-terminal-removed-meta-variables terminal)))
           expr ...)) ) )

    (define-record-type nonterminal-definition
      (make-nonterminal-definition name meta-variables production-definitions)
      nonterminal-definition?
      (name                   nonterminal-name)
      (meta-variables         nonterminal-meta-variables)
      (production-definitions nonterminal-production-definitions) )

    (define-syntax with-nonterminal-definition
      (syntax-rules ()
        ((_ nonterminal (name meta-variables production-definitions) expr ...)
         (let ((name                   (nonterminal-name                   nonterminal))
               (meta-variables         (nonterminal-meta-variables         nonterminal))
               (production-definitions (nonterminal-production-definitions nonterminal)))
           expr ...)) ) )

    (define-record-type nonterminal-modification
      (make-nonterminal-modification name
        added-meta-variables removed-meta-variables
        added-production-definitions removed-production-definitions )
      nonterminal-modification?
      (name                           modified-nonterminal-name)
      (added-meta-variables           modified-nonterminal-added-meta-variables)
      (removed-meta-variables         modified-nonterminal-removed-meta-variables)
      (added-production-definitions   modified-nonterminal-added-production-definitions)
      (removed-production-definitions modified-nonterminal-removed-production-definitions) )

    (define-syntax with-nonterminal-modification
      (syntax-rules ()
        ((_ nonterminal (name added-meta-variables removed-meta-variables added-production-definitions removed-production-definitions) expr ...)
         (let ((name                           (modified-nonterminal-name                           nonterminal))
               (added-meta-variables           (modified-nonterminal-added-meta-variables           nonterminal))
               (removed-meta-variables         (modified-nonterminal-removed-meta-variables         nonterminal))
               (added-production-definitions   (modified-nonterminal-added-production-definitions   nonterminal))
               (removed-production-definitions (modified-nonterminal-removed-production-definitions nonterminal)))
           expr ...)) ) )

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

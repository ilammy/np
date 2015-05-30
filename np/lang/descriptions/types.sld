(define-library (np lang descriptions types)
  ;;;
  ;;; Types used in processing of language definitions and descriptions
  ;;;
  (export terminal-definition terminal-definition?
          make-terminal-definition
          check-terminal-definition
          with-terminal-definition
          terminal-name
          terminal-predicate
          terminal-meta-variables

          terminal-modification terminal-modification?
          make-terminal-modification
          check-terminal-modification
          with-terminal-modification
          modified-terminal-name
          modified-terminal-added-meta-variables
          modified-terminal-removed-meta-variables

          nonterminal-definition nonterminal-definition?
          make-nonterminal-definition
          check-nonterminal-definition
          with-nonterminal-definition
          nonterminal-name
          nonterminal-meta-variables
          nonterminal-production-definitions

          nonterminal-modification nonterminal-modification?
          make-nonterminal-modification
          check-nonterminal-modification
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

  (import (scheme base)
          (only (srfi 1) filter))

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

    ;;;
    ;;; Type check helpers
    ;;;

    (define-syntax collect-errors
      (syntax-rules ()
        ((_ object clauses ...)
         (let ((errors '()))
           (collect* errors object clauses ...)
           (reverse errors) )) ) )

    (define-syntax collect*
      (syntax-rules ()
        ((_ errors object ((predicate expr) kind clauses ...) ...)
         (begin
           (let ((value expr))
             (if (predicate value)
                 (collect* errors object clauses ...)
                 (set! errors (cons (lang-error kind object value) errors)) ) ) ...)) ) )

    (define (meta-variable? x)
      (symbol? x) )

    (define (production? x)
      (or (null? x)
          (symbol? x)
          (and (pair? x)
               (production? (car x))
               (production? (cdr x)) ) ) )

    (define (invalid-meta-variables list)
      (filter (lambda (x) (not (meta-variable? x))) list) )

    (define (invalid-productions list)
      (filter (lambda (x) (not (production? x))) list) )

    ;;;
    ;;; Type checks
    ;;;

    (define (check-terminal-definition x)
      (collect-errors x
        ((terminal-definition? x)                        'type:terminal-definition
          ((symbol? (terminal-name x))                   'type:terminal-name)
          ((procedure? (terminal-predicate x))           'type:terminal-predicate)
          ((list? (terminal-meta-variables x))           'type:terminal-meta-var-list
            ((null? (invalid-meta-variables (terminal-meta-variables x)))
                                                         'type:terminal-meta-var ) ) ) ) )

    (define (check-terminal-modification x)
      (collect-errors x
        ((terminal-modification? x)                              'type:terminal-modification
          ((symbol? (modified-terminal-name x))                  'type:terminal-name)
          ((list? (modified-terminal-added-meta-variables x))    'type:terminal-added-meta-var-list
            ((null? (invalid-meta-variables (modified-terminal-added-meta-variables x)))
                                                                 'type:terminal-added-meta-var ) )
          ((list? (modified-terminal-removed-meta-variables x))  'type:terminal-removed-meta-var-list
            ((null? (invalid-meta-variables (modified-terminal-removed-meta-variables x)))
                                                                 'type:terminal-removed-meta-var ) ) ) ) )

    (define (check-nonterminal-definition x)
      (collect-errors x
        ((nonterminal-definition? x)                                'type:nonterminal-definition
          ((symbol? (nonterminal-name x))                           'type:nonterminal-name)
          ((list? (nonterminal-meta-variables x))                   'type:nonterminal-meta-var-list
            ((null? (invalid-meta-variables (nonterminal-meta-variables x)))
                                                                    'type:nonterminal-meta-var ) )
          ((list? (nonterminal-production-definitions x))           'type:nonterminal-production-list
            ((null? (invalid-productions (nonterminal-production-definitions x)))
                                                                    'type:nonterminal-production ) ) ) ) )

    (define (check-nonterminal-modification x)
      (collect-errors x
        ((nonterminal-modification? x)                                      'type:nonterminal-modification
          ((symbol? (modified-nonterminal-name x))                          'type:nonterminal-name)
          ((list? (modified-nonterminal-added-meta-variables x))            'type:nonterminal-added-meta-var-list
            ((null? (invalid-meta-variables (modified-nonterminal-added-meta-variables x)))
                                                                            'type:nonterminal-added-meta-var) )
          ((list? (modified-nonterminal-removed-meta-variables x))          'type:nonterminal-removed-meta-var-list
            ((null? (invalid-meta-variables (modified-nonterminal-removed-meta-variables x)))
                                                                            'type:nonterminal-removed-meta-var) )
          ((list? (modified-nonterminal-added-production-definitions x))    'type:nonterminal-added-production-list
            ((null? (invalid-productions (modified-nonterminal-added-production-definitions x)))
                                                                            'type:nonterminal-added-production) )
          ((list? (modified-nonterminal-removed-production-definitions x))  'type:nonterminal-removed-production-list
            ((null? (invalid-productions (modified-nonterminal-removed-production-definitions x)))
                                                                            'type:nonterminal-removed-production) ) ) ) )

) )

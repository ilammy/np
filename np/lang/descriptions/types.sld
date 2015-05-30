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
          (only (srfi 1) every filter remove))

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

    (define-syntax check
      (syntax-rules (else)
        ((check object type-predicate?
           (with-destructuring-bind (fields ...) clauses ...)
           (else type-kind
             (constructor default-fields ...) ) )
         (if (type-predicate? object)
             (with-destructuring-bind object (fields ...)
               (check-clause* object clauses) ...
               (constructor fields ...) )
             (begin
               (raise-continuable (lang-error type-kind object))
               (constructor default-fields ...) ) ) ) ) )

    (define-syntax check-clause*
      (syntax-rules ()
        ((_ object ((predicate? field) kind default))
         (unless (predicate? kind)
           (raise-continuable (lang-error kind object field))
           (set! field default) ))
        ((_ object ((predicate? field) kind default error-object))
         (unless (predicate? kind)
           (raise-continuable (lang-error kind object error-object))
           (set! field default) )) ) )

    (define (name? x)
      (symbol? x) )

    (define (meta-variable? x)
      (symbol? x) )

    (define (production? x)
      (or (null? x)
          (symbol? x)
          (and (pair? x)
               (production? (car x))
               (production? (cdr x)) ) ) )

    (define (meta-vars? list)
      (every meta-variable? list) )

    (define (valid-meta-vars list)
      (filter meta-variable? list) )

    (define (invalid-meta-vars list)
      (remove meta-variable? list) )

    (define (productions? list)
      (every production? list) )

    (define (valid-productions list)
      (filter production? list) )

    (define (invalid-productions list)
      (remove production? list) )

    (define (proper-part list)
      (let loop ((list list) (res '()))
        (if (pair? list)
            (loop (cdr list)
                  (cons (car list) res) )
            (reverse res) ) ) )

    ;;;
    ;;; Type checks
    ;;;

    (define invalid-name '<invalid>)
    (define invalid-predicate (lambda (x) #f))

    (define (check-terminal-definition object)
      (check object terminal-definition?
        (with-terminal-definition (name predicate meta-vars)
          ((name? name)           'type:terminal-name
                                   invalid-name )
          ((procedure? predicate) 'type:terminal-predicate
                                   invalid-predicate )
          ((list? meta-vars)      'type:terminal-meta-var-list
                                   (proper-part meta-vars) )
          ((meta-vars? meta-vars) 'type:terminal-meta-var
                                   (valid-meta-vars meta-vars)
                                   (invalid-meta-vars meta-vars) ) )
        (else 'type:terminal-definition
          (make-terminal-definition invalid-name invalid-predicate '()) ) ) )

    (define (check-terminal-modification object)
      (check object terminal-modification?
        (with-terminal-modification (name meta-vars+ meta-vars-)
          ((name? name)            'type:terminal-name
                                    invalid-name )
          ((list? meta-vars+)      'type:terminal-added-meta-var-list
                                    (proper-part meta-vars+) )
          ((meta-vars? meta-vars+) 'type:terminal-added-meta-var
                                    (valid-meta-vars meta-vars+)
                                    (invalid-meta-vars meta-vars+) )
          ((list? meta-vars-)      'type:terminal-removed-meta-var-list
                                    (proper-part meta-vars-) )
          ((meta-vars? meta-vars-) 'type:terminal-removed-meta-var
                                    (valid-meta-vars meta-vars-)
                                    (invalid-meta-vars meta-vars-) ) )
        (else 'type:terminal-modification
          (make-terminal-modification invalid-name '() '()) ) ) )

    (define (check-nonterminal-definition object)
      (check object nonterminal-definition?
        (with-nonterminal-definition (name meta-vars productions)
          ((name? name)               'type:nonterminal-name
                                       invalid-name )
          ((list? meta-vars)          'type:nonterminal-meta-var-list
                                       (proper-part meta-vars) )
          ((meta-vars? meta-vars)     'type:nonterminal-meta-var
                                       (valid-meta-vars meta-vars)
                                       (invalid-meta-vars meta-vars) )
          ((list? productions)        'type:nonterminal-production-list
                                       (proper-part productions) )
          ((productions? productions) 'type:nonterminal-production
                                       (valid-productions productions)
                                       (invalid-productions productions) ) )
        (else 'type:nonterminal-definition
          (make-nonterminal-definition invalid-name '() '()) ) ) )

    (define (check-nonterminal-modification object)
      (check object nonterminal-modification?
        (with-nonterminal-modification (name meta-vars+ meta-vars- productions+ productions-)
          ((name? name)                'type:nonterminal-name
                                        invalid-name )
          ((list? meta-vars+)          'type:nonterminal-added-meta-var-list
                                        (proper-part meta-vars+) )
          ((meta-vars? meta-vars+)     'type:nonterminal-added-meta-var
                                        (valid-meta-vars meta-vars+)
                                        (invalid-meta-vars meta-vars+) )
          ((list? meta-vars-)          'type:nonterminal-removed-meta-var-list
                                        (proper-part meta-vars-) )
          ((meta-vars? meta-vars-)     'type:nonterminal-removed-meta-var
                                        (valid-meta-vars meta-vars-)
                                        (invalid-meta-vars meta-vars-) )
          ((list? productions+)        'type:nonterminal-added-production-list
                                        (proper-part productions+) )
          ((productions? productions+) 'type:nonterminal-added-production
                                        (valid-productions productions+)
                                        (invalid-productions productions+) )
          ((list? productions-)        'type:nonterminal-removed-production-list
                                        (proper-part productions-) )
          ((productions? productions-) 'type:nonterminal-removed-production
                                        (valid-productions productions-)
                                        (invalid-productions productions-) ) )
        (else 'type:nonterminal-modification
          (make-nonterminal-modification invalid-name '() '() '() '()) ) ) )

) )

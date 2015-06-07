(define-library (np lang descriptions syntax)
  ;;;
  ;;; Syntax checking of language definitions
  ;;;
  (export check-terminal-definition
          check-terminal-modification
          check-terminal-removal
          check-nonterminal-definition
          check-nonterminal-modification
          check-nonterminal-removal)

  (import (scheme base)
          (only (srfi 1) every filter remove)
          (np lang descriptions types))

  (begin
    ;;;
    ;;; Helpers
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
    ;;; Syntax checks
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

    (define (check-terminal-removal object)
      (if (name? object)
          object
          (begin
            (raise-continuable (lang-error 'type:terminal-removal object))
            invalid-name ) ) )

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

    (define (check-nonterminal-removal object)
      (if (name? object)
          object
          (begin
            (raise-continuable (lang-error 'type:nonterminal-removal object))
            invalid-name ) ) )

) )

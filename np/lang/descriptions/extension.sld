(define-library (np lang descriptions extension)
  ;;;
  ;;; Language extending
  ;;;
  (export append-terminal-definitions
          remove-terminal-definitions
          modify-terminal-definitions

          append-nonterminal-definitions
          remove-nonterminal-definitions
          modify-nonterminal-definitions)

  (import (scheme base)
          (srfi 1)   ; list library
          (np lang descriptions definitions)
          (np lang descriptions errors)
          (np lang descriptions unification))

  (begin
    ;;;
    ;;; Helpers
    ;;;

    (define (find-duplicates list equal?)
      (let loop ((duplicates '()) (list list))
        (if (null? list)
            duplicates
            (if (member (car list) (cdr list) equal?)
                (loop (cons (car list) duplicates) (cdr list))
                (loop duplicates (cdr list)) ) ) ) )

    (define (find-missing present removed equal?)
      (remove
       (lambda (x) (member x present equal?))
       removed ) )

    (define (apply-modifications present added removed equal?)
      (append added
       (remove
        (lambda (x) (member x removed equal?))
        present ) ) )

    (define (check-missing/duplicates! present removed equal? context kind-duplicates kind-missing)
      (let ((duplicates (find-duplicates removed equal?)))
        (unless (null? duplicates)
          (raise-continuable (lang-error kind-duplicates context duplicates)) ) )
      (let ((missing (find-missing present removed equal?)))
        (unless (null? missing)
          (raise-continuable (lang-error kind-missing context missing)) ) ) )

    ;;;
    ;;; Terminals
    ;;;

    (define (append-terminal-definitions old-terminals new-terminals)
      (append old-terminals new-terminals) )

    (define (remove-terminal-definitions terminals removed-names)
      (check-missing/duplicates!
        (map terminal-name terminals)
        removed-names eq? #f
        'ext:term:remove-duplicate
        'ext:term:remove-missing )
      (remove
        (lambda (terminal) (memq (terminal-name terminal) removed-names))
        terminals ) )

    (define (modify-terminal-definitions terminals modifications)
      (check-missing/duplicates!
        (map terminal-name terminals)
        (map modified-terminal-name modifications)
        eq? #f
        'ext:term:modify-duplicate
        'ext:term:modify-missing )
      (map
        (lambda (terminal)
          (define (with-name name)
            (lambda (modification)
              (eq? (modified-terminal-name modification) name) ) )
          (let ((modification (find (with-name (terminal-name terminal)) modifications)))
            (if modification
                (modified-terminal terminal modification)
                terminal ) ) )
        terminals ) )

    (define (modified-terminal terminal modification)
      (with-terminal-definition terminal (name predicate meta-vars)
        (with-terminal-modification modification (name meta-vars+ meta-vars-)
          (check-missing/duplicates!
            meta-vars meta-vars- eq? modification
            'ext:term:modify:meta-var-remove-duplicate
            'ext:term:modify:meta-var-remove-missing )
          (make-terminal-definition name predicate
           (apply-modifications meta-vars meta-vars+ meta-vars- eq?) ) ) ) )

    ;;;
    ;;; Nonterminals
    ;;;

    (define (append-nonterminal-definitions old-nonterminals new-nonterminals)
      (append old-nonterminals new-nonterminals) )

    (define (remove-nonterminal-definitions nonterminals removed-names)
      (check-missing/duplicates!
        (map nonterminal-name nonterminals)
        removed-names eq? #f
        'ext:nonterm:remove-duplicate
        'ext:nonterm:remove-missing )
      (remove
        (lambda (nonterminal) (memq (nonterminal-name nonterminal) removed-names))
        nonterminals ) )

    (define (modify-nonterminal-definitions terminals nonterminals modifications)
      (check-missing/duplicates!
        (map nonterminal-name nonterminals)
        (map modified-nonterminal-name modifications)
        eq? #f
        'ext:nonterm:modify-duplicate
        'ext:nonterm:modify-missing )
      (let ((productions-equal? (equality-for-mapping (make-meta-var-mapping terminals nonterminals))))
        (map
          (lambda (nonterminal)
            (define (with-name name)
              (lambda (modification)
                (eq? (modified-nonterminal-name modification) name) ) )
            (let ((modification (find (with-name (nonterminal-name nonterminal)) modifications)))
              (if modification
                  (modified-nonterminal nonterminal modification productions-equal?)
                  nonterminal ) ) )
          nonterminals ) ) )

    (define (modified-nonterminal nonterminal modification productions-equal?)
      (with-nonterminal-definition nonterminal (name meta-vars productions)
        (with-nonterminal-modification modification (name meta-vars+ meta-vars- productions+ productions-)
          (check-missing/duplicates!
            meta-vars meta-vars- eq? modification
            'ext:nonterm:modify:meta-var-remove-duplicate
            'ext:nonterm:modify:meta-var-remove-missing )
          (check-missing/duplicates!
            productions productions- productions-equal? modification
            'ext:nonterm:modify:production-remove-duplicate
            'ext:nonterm:modify:production-remove-missing )
          (make-nonterminal-definition name
           (apply-modifications meta-vars meta-vars+ meta-vars- eq?)
           (apply-modifications productions productions+ productions- productions-equal?) ) ) ) )

) )

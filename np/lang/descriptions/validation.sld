(define-library (np lang descriptions validation)
  ;;;
  ;;; Definition validation
  ;;;
  (export validate-terminal-definition!
          validate-nonterminal-definition!
          validate-meta-variables!
          validate-name-assignment!
          validate-meta-variable-assignment!
          validate-production-duplicates!
          validate-production-syntax!)

  (import (scheme base)
          (srfi 1)   ; list library
          (srfi 113) ; sets and bags
          (srfi 114) ; comparators
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

    ;;;
    ;;; Terminals
    ;;;

    (define (validate-terminal-definition! terminal)
      (when (null? (terminal-meta-variables terminal))
        (raise-continuable (lang-error 'validate:term:no-meta-vars terminal)) ) )

    ;;;
    ;;; Nonterminals
    ;;;

    (define (validate-nonterminal-definition! nonterminal)
      (when (null? (nonterminal-production-definitions nonterminal))
        (raise-continuable (lang-error 'validate:nonterm:no-productions nonterminal)) ) )

    ;;;
    ;;; Meta-variables
    ;;;

    (define (validate-meta-variables! meta-vars)
      (let ((duplicates (find-duplicates meta-vars eq?)))
        (unless (null? duplicates)
          (raise-continuable (lang-error 'validate:meta-vars:duplicate duplicates)) ) )
      (let ((reserved (filter reserved? meta-vars)))
        (unless (null? reserved)
          (raise-continuable (lang-error 'validate:meta-vars:reserved reserved)) ) )
      (let ((suffixed (filter suffixed? meta-vars)))
        (unless (null? suffixed)
          (raise-continuable (lang-error 'validate:meta-vars:suffix suffixed)) ) ) )

    (define (reserved? meta-var)
      (or (eq? meta-var 'maybe)
          (eq? meta-var '!..)
          (eq? meta-var '...) ) )

    (define (suffixed? meta-var)
      (not (eq? meta-var (trim-meta-var-name meta-var))) )

    ;;;
    ;;; Naming
    ;;;

    (define (validate-name-assignment! terminals nonterminals)
      (let* ((counts (count-unique-names terminals nonterminals))
             (ambiguous (find-non-unique-names counts)))
        (unless (null? ambiguous)
          (raise-continuable (lang-error 'validate:names:ambiguous ambiguous)) ) ) )

    (define (validate-meta-variable-assignment! terminals nonterminals)
      (let* ((counts (count-unique-meta-vars terminals nonterminals))
             (ambiguous (find-non-unique-names counts)))
        (unless (null? ambiguous)
          (raise-continuable (lang-error 'validate:meta-vars:ambiguous ambiguous)) ) ) )

    (define (count-unique-names terminals nonterminals)
      (let* ((bag (bag symbol-comparator))
             (bag (list->bag! bag (map terminal-name terminals)))
             (bag (list->bag! bag (map nonterminal-name nonterminals))))
        bag ) )

    (define (count-unique-meta-vars terminals nonterminals)
      (let* ((bag (bag symbol-comparator))
             (bag (lists->bag! bag (map unique-terminal-meta-vars terminals)))
             (bag (lists->bag! bag (map unique-nonterminal-meta-vars nonterminals))))
        bag ) )

    (define (lists->bag! bag lists)
      (let loop ((bag bag) (lists lists))
        (if (null? lists)
            bag
            (loop (list->bag! bag (car lists)) (cdr lists)) ) ) )

    (define (unique-terminal-meta-vars terminal)
      (delete-duplicates (terminal-meta-variables terminal) eq?) )

    (define (unique-nonterminal-meta-vars nonterminal)
      (delete-duplicates (nonterminal-meta-variables nonterminal) eq?) )

    (define (find-non-unique-names name-count-bag)
      (bag-fold-unique
       (lambda (name count ambiguous)
         (if (> count 1)
             (cons name ambiguous)
             ambiguous ) )
       '() name-count-bag ) )

    ;;;
    ;;; Productions
    ;;;

    (define (validate-production-duplicates! mapping productions)
      (let ((duplicates (find-duplicates productions (equality-for-mapping mapping))))
        (unless (null? duplicates)
          (raise-continuable (lang-error 'validate:productions:duplicate duplicates)) ) ) )

    ;; Production grammar
    ;; ------------------
    ;;
    ;; Terminals:
    ;;
    ;;   null     - empty list ()
    ;;   maybe    - symbol "maybe"
    ;;   repeat   - symbol "!.." or "..."
    ;;   meta-var - symbol bound to meta-variables
    ;;   keyword  - any other symbol
    ;;
    ;; Productions:
    ;;
    ;;   production ::= meta-var
    ;;               |  (keyword . term-list)
    ;;               |  term-cons
    ;;
    ;;    term-list ::= null
    ;;               |  term-cons
    ;;
    ;;    term-cons ::= (term . term-list)
    ;;               |  (term repeat . term-list)
    ;;
    ;;         term ::= meta-var
    ;;               |  (maybe meta-var)
    ;;               |  term-cons

    (define (validate-production-syntax! mapping production)

      (define (repeat? x)
        (or (eq? x '!..) (eq? x '...)) )

      (define (maybe? x)
        (eq? x 'maybe) )

      (define (meta-var? x)
        (and (symbol? x)
             (meta-var-mapped? mapping x) ) )

      (define (keyword? x)
        (and (symbol? x)
             (not (repeat? x))
             (not (meta-var? x)) ) )

      (define (validate-production! x)
        (cond
          ((symbol? x) (validate-meta-var! x))
          ((pair? x)
           (if (keyword? (car x))
               (validate-term-list! (cdr x))
               (validate-term-cons! x) ) )
          (else (raise-continuable (lang-error 'validate:productions:syntax:invalid production x))) ) )

      (define (validate-meta-var! x)
        ; (assert (symbol? x))
        (unless (meta-var? x)
          (raise-continuable (lang-error 'validate:productions:syntax:unknown production x)) ) )

      (define (validate-term-list! x)
        (cond
          ((null? x) #t)
          ((pair? x) (validate-term-cons! x))
          (else (raise-continuable (lang-error 'validate:productions:syntax:invalid production x))) ) )

      (define (validate-term-cons! x)
        ; (assert (pair? x))
        (validate-term! x (car x))
        (if (and (pair? (cdr x)) (repeat? (cadr x)))
            (validate-term-list! (cddr x))
            (validate-term-list! (cdr x)) ) )

      (define (validate-term! e x)
        (cond
          ((symbol? x)
           (if (repeat? x)
               (raise-continuable (lang-error 'validate:productions:syntax:repeat production e))
               (validate-meta-var! x) ) )
          ((pair? x)
           (if (maybe? (car x))
               (validate-maybe! x)
               (validate-term-cons! x) ) )
          (else (raise-continuable (lang-error 'validate:productions:syntax:invalid production e x))) ) )

      (define (validate-maybe! x)
        ; (assert (pair? x) (eq? (car x) 'maybe))
        (if (and (pair? (cdr x))
                 (null? (cddr x))
                 (symbol? (cadr x)) )
            (validate-meta-var! (cadr x))
            (raise-continuable (lang-error 'validate:productions:syntax:maybe production x)) ) )

      (validate-production! production) )

) )

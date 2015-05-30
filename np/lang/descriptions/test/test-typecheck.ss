(import (scheme base)
        (np lang descriptions types)
        (np lang descriptions test-utils)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define non-symbols     `(42 "str" ,null? #\N () (a . d) #f (1 2 3) #(name)))
(define non-procedures  `(42 "str" null? #\N () (a . d) #f (1 2 3) #(name)))
(define non-lists       `(42 "str" null? ,null? #\N (a . d) #f #(x y z)))
(define non-productions `(42 "str" ,null? #\N #f #(name)))

(define invalid-names               (map list non-symbols))
(define invalid-predicates          (map list non-procedures))
(define invalid-meta-variables      (map list (map list non-symbols)))
(define invalid-meta-variable-lists (map list non-lists))
(define invalid-productions         (map list (map list non-productions)))
(define invalid-production-lists    (map list non-productions))
(define invalid-productions-nested `((((foo bar 42))) (((x (y . #(z))))) (((h i j ... k 4))) (((m . #\n)))))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (typecheck:terminals "Type checking of terminals")

  (define (make-checked-definition name predicate meta-vars)
    (check-terminal-definition
     (make-terminal-definition name predicate meta-vars) ) )

  (define-test ("Normal terminal")
    (assert-null (make-checked-definition 'number number? '(n))) )

  (define-test ("Empty meta-variables")
    (assert-null (make-checked-definition 'number number? '())) )

  (define-test ("Empty name")
    (assert-null (make-checked-definition '|| null? '())) )

  (define-test ("Senseless predicate procedure")
    (assert-null (make-checked-definition 'number * '(n))) )

  (define-test ("Terminal type")
    (let ((errors (check-terminal-definition 42)))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:terminal-definition (car errors)) ) )

  (define-test ("Terminal name" name)
    #(invalid-names)
    (let ((errors (make-checked-definition name null? '())))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:terminal-name (car errors)) ) )

  (define-test ("Terminal predicate" predicate)
    #(invalid-predicates)
    (let ((errors (make-checked-definition 'number predicate '())))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:terminal-predicate (car errors)) ) )

  (define-test ("Terminal meta-variable list" meta-vars)
    #(invalid-meta-variable-lists)
    (let ((errors (make-checked-definition 'number number? meta-vars)))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:terminal-meta-var-list (car errors)) ) )

  (define-test ("Terminal meta-variables" meta-vars)
    #(invalid-meta-variables)
    (let ((errors (make-checked-definition 'number number? meta-vars)))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:terminal-meta-var (car errors)) ) )

  (define-test ("Terminal name, predicate, meta-vars")
    (let ((errors (make-checked-definition 1 2 '(3))))
      (assert-= 3 (length errors))
      (assert-lang-error 'type:terminal-name      (list-ref errors 0))
      (assert-lang-error 'type:terminal-predicate (list-ref errors 1))
      (assert-lang-error 'type:terminal-meta-var  (list-ref errors 2)) ) )
)
(verify-test-case! typecheck:terminals)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (typecheck:terminal-mods "Type checking of terminal modifications")

  (define (make-checked-modification name added-meta-vars removed-meta-vars)
    (check-terminal-modification
     (make-terminal-modification name added-meta-vars removed-meta-vars) ) )

  (define-test ("Normal terminal modification")
    (assert-null (make-checked-modification 'number '(n) '(m))) )

  (define-test ("Empty meta-variables")
    (assert-null (make-checked-modification 'number '() '())) )

  (define-test ("Empty name")
    (assert-null (make-checked-modification '|| '() '())) )

  (define-test ("Terminal type")
    (let ((errors (check-terminal-modification 42)))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:terminal-modification (car errors)) ) )

  (define-test ("Terminal name" name)
    #(invalid-names)
    (let ((errors (make-checked-modification name '() '())))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:terminal-name (car errors)) ) )

  (define-test ("Terminal meta-variable list (addition)" meta-vars)
    #(invalid-meta-variable-lists)
    (let ((errors (make-checked-modification 'number meta-vars '())))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:terminal-added-meta-var-list (car errors)) ) )

  (define-test ("Terminal meta-variable list (removal)" meta-vars)
    #(invalid-meta-variable-lists)
    (let ((errors (make-checked-modification 'number '() meta-vars)))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:terminal-removed-meta-var-list (car errors)) ) )

  (define-test ("Terminal meta-variables (addition)" meta-vars)
    #(invalid-meta-variables)
    (let ((errors (make-checked-modification 'number meta-vars '())))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:terminal-added-meta-var (car errors)) ) )

  (define-test ("Terminal meta-variables (removal)" meta-vars)
    #(invalid-meta-variables)
    (let ((errors (make-checked-modification 'number '() meta-vars)))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:terminal-removed-meta-var (car errors)) ) )

  (define-test ("Terminal name, meta-vars and list")
    (let ((errors (make-checked-modification 1 '(2 3) '4)))
      (assert-= 3 (length errors))
      (assert-lang-error 'type:terminal-name                  (list-ref errors 0))
      (assert-lang-error 'type:terminal-added-meta-var        (list-ref errors 1))
      (assert-lang-error 'type:terminal-removed-meta-var-list (list-ref errors 2)) ) )

  (define-test ("Terminal name, meta-vars (both types)")
    (let ((errors (make-checked-modification 1 '(2 3) '(4))))
      (assert-= 3 (length errors))
      (assert-lang-error 'type:terminal-name             (list-ref errors 0))
      (assert-lang-error 'type:terminal-added-meta-var   (list-ref errors 1))
      (assert-lang-error 'type:terminal-removed-meta-var (list-ref errors 2)) ) )
)
(verify-test-case! typecheck:terminal-mods)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (typecheck:nonterminals "Type checking of nonterminals")

  (define (make-checked-definition name meta-vars productions)
    (check-nonterminal-definition
     (make-nonterminal-definition name meta-vars productions) ) )

  (define-test ("Normal nonterminal")
    (assert-null (make-checked-definition 'Number '(Num) '(N (n n)))) )

  (define-test ("Empty meta-variables")
    (assert-null (make-checked-definition 'Number '() '(N (n n)))) )

  (define-test ("Empty productions")
    (assert-null (make-checked-definition 'Number '() '())) )

  (define-test ("Empty name")
    (assert-null (make-checked-definition '|| '() '())) )

  (define-test ("Nonterminal type")
    (let ((errors (check-nonterminal-definition 42)))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-definition (car errors)) ) )

  (define-test ("Nonterminal name" name)
    #(invalid-names)
    (let ((errors (make-checked-definition name '() '())))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-name (car errors)) ) )

  (define-test ("Nonterminal meta-variable list" meta-vars)
    #(invalid-meta-variable-lists)
    (let ((errors (make-checked-definition 'Number meta-vars '())))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-meta-var-list (car errors)) ) )

  (define-test ("Nonterminal meta-variables" meta-vars)
    #(invalid-meta-variables)
    (let ((errors (make-checked-definition 'Number meta-vars '())))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-meta-var (car errors)) ) )

  (define-test ("Nonterminal production list" productions)
    #(invalid-production-lists)
    (let ((errors (make-checked-definition 'Number '() productions)))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-production-list (car errors)) ) )

  (define-test ("Nonterminal productions" productions)
    #(invalid-productions)
    (let ((errors (make-checked-definition 'Number '() productions)))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-production (car errors)) ) )

  (define-test ("Nonterminal productions nested" productions)
    #(invalid-productions-nested)
    (let ((errors (make-checked-definition 'Number '() productions)))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-production (car errors)) ) )

  (define-test ("Nonterminal name, meta-vars and productions")
    (let ((errors (make-checked-definition 1 '(2) '(3))))
      (assert-= 3 (length errors))
      (assert-lang-error 'type:nonterminal-name       (list-ref errors 0))
      (assert-lang-error 'type:nonterminal-meta-var   (list-ref errors 1))
      (assert-lang-error 'type:nonterminal-production (list-ref errors 2)) ) )
)
(verify-test-case! typecheck:nonterminals)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (typecheck:nonterminal-mods "Type checking of nonterminal modifications")

  (define (make-checked-modification name added-meta-vars removed-meta-vars added-productions removed-productions)
    (check-nonterminal-modification
     (make-nonterminal-modification name added-meta-vars removed-meta-vars added-productions removed-productions) ) )

  (define-test ("Normal nonterminal modification")
    (assert-null (make-checked-modification 'Number '(Num) '(Number) '(n (r i)) '((C r i)))) )

  (define-test ("Empty meta-variables")
    (assert-null (make-checked-modification 'Number '() '() '(n (r i)) '((C r i)))) )

  (define-test ("Empty productions")
    (assert-null (make-checked-modification 'Number '() '() '() '())) )

  (define-test ("Empty name")
    (assert-null (make-checked-modification '|| '() '() '() '())) )

  (define-test ("Nonterminal type")
    (let ((errors (check-nonterminal-modification 42)))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-modification (car errors)) ) )

  (define-test ("Nonterminal name" name)
    #(invalid-names)
    (let ((errors (make-checked-modification name '() '() '() '())))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-name (car errors)) ) )

  (define-test ("Nonterminal meta-variable list (addition)" meta-vars)
    #(invalid-meta-variable-lists)
    (let ((errors (make-checked-modification 'Number meta-vars '() '() '())))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-added-meta-var-list (car errors)) ) )

  (define-test ("Nonterminal meta-variable list (removal)" meta-vars)
    #(invalid-meta-variable-lists)
    (let ((errors (make-checked-modification 'Number '() meta-vars '() '())))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-removed-meta-var-list (car errors)) ) )

  (define-test ("Nonterminal meta-variables (addition)" meta-vars)
    #(invalid-meta-variables)
    (let ((errors (make-checked-modification 'Number meta-vars '() '() '())))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-added-meta-var (car errors)) ) )

  (define-test ("Nonterminal meta-variables (removal)" meta-vars)
    #(invalid-meta-variables)
    (let ((errors (make-checked-modification 'Number '() meta-vars '() '())))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-removed-meta-var (car errors)) ) )

  (define-test ("Nonterminal production list (addition)" productions)
    #(invalid-production-lists)
    (let ((errors (make-checked-modification 'Number '() '() productions '())))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-added-production-list (car errors)) ) )

  (define-test ("Nonterminal production list (removal)" productions)
    #(invalid-production-lists)
    (let ((errors (make-checked-modification 'Number '() '() '() productions)))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-removed-production-list (car errors)) ) )

  (define-test ("Nonterminal productions (addition)" productions)
    #(invalid-productions)
    (let ((errors (make-checked-modification 'Number '() '() productions '())))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-added-production (car errors)) ) )

  (define-test ("Nonterminal productions (removal)" productions)
    #(invalid-productions)
    (let ((errors (make-checked-modification 'Number '() '() '() productions)))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-removed-production (car errors)) ) )

  (define-test ("Nonterminal productions nested (addition)" productions)
    #(invalid-productions-nested)
    (let ((errors (make-checked-modification 'Number '() '() productions '())))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-added-production (car errors)) ) )

  (define-test ("Nonterminal productions nested (removal)" productions)
    #(invalid-productions-nested)
    (let ((errors (make-checked-modification 'Number '() '() '() productions)))
      (assert-= 1 (length errors))
      (assert-lang-error 'type:nonterminal-removed-production (car errors)) ) )

  (define-test ("Nonterminal name, meta-vars, productions and list")
    (let ((errors (make-checked-modification 1 '(2 3) '4 '5 '(6 7))))
      (assert-= 5 (length errors))
      (assert-lang-error 'type:nonterminal-name                  (list-ref errors 0))
      (assert-lang-error 'type:nonterminal-added-meta-var        (list-ref errors 1))
      (assert-lang-error 'type:nonterminal-removed-meta-var-list (list-ref errors 2))
      (assert-lang-error 'type:nonterminal-added-production-list (list-ref errors 3))
      (assert-lang-error 'type:nonterminal-removed-production    (list-ref errors 4)) ) )

  (define-test ("Nonterminal name, meta-vars, productions (both types)")
    (let ((errors (make-checked-modification 1 '(2 3) '(4) '(5 6) '(7 #(8)))))
      (assert-= 5 (length errors))
      (assert-lang-error 'type:nonterminal-name               (list-ref errors 0))
      (assert-lang-error 'type:nonterminal-added-meta-var     (list-ref errors 1))
      (assert-lang-error 'type:nonterminal-removed-meta-var   (list-ref errors 2))
      (assert-lang-error 'type:nonterminal-added-production   (list-ref errors 3))
      (assert-lang-error 'type:nonterminal-removed-production (list-ref errors 4)) ) )
)
(verify-test-case! typecheck:nonterminal-mods)

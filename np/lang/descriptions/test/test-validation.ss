(import (scheme base)
        (np lang descriptions validation)
        (np lang descriptions definitions)
        (np lang descriptions errors)
        (np lang descriptions unification)
        (np lang descriptions test utils)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define (collect-errors body handler)
  (let ((errors '()))
    (with-exception-handler
      (lambda (condition)
        (if (lang-error? condition)
            (set! errors (cons condition errors))
            (raise condition) ) )
      body )
    (handler (reverse errors)) ) )

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (validation:terminals "Validation of terminal definitions")

  (define number (make-terminal-definition 'number number? '(n)))
  (define void   (make-terminal-definition 'void   null?   '()))

  (define-test ("Normal definition")
    (validate-terminal-definition! number) )

  (define-test ("Minimal meta-variable quantity")
    (collect-errors
     (lambda ()
       (validate-terminal-definition! void) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:term:no-meta-vars (car errors)) ) ) )
)
(verify-test-case! validation:terminals)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (validation:nonterminals "Validation of nonterminal definitions")

  (define Number (make-nonterminal-definition 'Number '(N)   '(i (i i) (/ i i))))
  (define Void   (make-nonterminal-definition 'Void   '()    '(())))
  (define Object (make-nonterminal-definition 'Object '(Obj) '()))

  (define-test ("Normal definition")
    (validate-nonterminal-definition! Number) )

  (define-test ("Definition without meta-variables")
    (validate-nonterminal-definition! Void) )

  (define-test ("Minimal production quantity")
    (collect-errors
     (lambda ()
       (validate-nonterminal-definition! Object) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:nonterm:no-productions (car errors)) ) ) )
)
(verify-test-case! validation:nonterminals)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (validation:meta-vars "Validation of meta-variables")

  (define-test ("Empty list")
    (validate-meta-variables! '()) )

  (define-test ("Normal variables")
    (validate-meta-variables! '(a b c)) )

  (define-test ("Quirky empty symbol")
    (validate-meta-variables! '(||)) )

  (define-test ("Duplicate meta-variables")
    (collect-errors
     (lambda ()
       (validate-meta-variables! '(n n o m m)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:meta-vars:duplicate (car errors)) ) ) )

  (define-test ("Cannot have suffixes" meta-var)
    #('((a*) (b123) (c++) (d?) (e6^) (*)))
    (collect-errors
     (lambda ()
       (validate-meta-variables! (list meta-var)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:meta-vars:suffix (car errors)) ) ) )

  (define-test ("Cannot have reserved names" meta-var)
    #('((maybe) (...) (!..)))
    (collect-errors
     (lambda ()
       (validate-meta-variables! (list meta-var)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:meta-vars:reserved (car errors)) ) ) )

  (define-test ("Worst nightmare")
    (collect-errors
     (lambda ()
       (validate-meta-variables! '(maybe !? !?)) )
     (lambda (errors)
       (assert-= 3 (length errors))
       (assert-lang-error 'validate:meta-vars:duplicate (list-ref errors 0))
       (assert-lang-error 'validate:meta-vars:reserved  (list-ref errors 1))
       (assert-lang-error 'validate:meta-vars:suffix    (list-ref errors 2)) ) ) )
)
(verify-test-case! validation:meta-vars)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (validation:name-assignment "Validation of name uniqueness")

  (define term1  (make-terminal-definition 'term1 null? '()))
  (define term21 (make-terminal-definition 'term2 null? '()))
  (define term22 (make-terminal-definition 'term2 null? '()))
  (define term3  (make-terminal-definition 'nonterm1 null? '()))

  (define nonterm1  (make-nonterminal-definition 'nonterm1 '() '()))
  (define nonterm21 (make-nonterminal-definition 'nonterm2 '() '()))
  (define nonterm22 (make-nonterminal-definition 'nonterm2 '() '()))
  (define nonterm3  (make-nonterminal-definition 'term1    '() '()))

  (define-test ("Empty list")
    (validate-name-assignment! '() '()) )

  (define-test ("Normal case")
    (validate-name-assignment! (list term1 term21) (list nonterm1 nonterm22)) )

  (define-test ("Terminals have non-unique assignments")
    (collect-errors
     (lambda ()
       (validate-name-assignment! (list term21 term22) '()) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:names:ambiguous (car errors)) ) ) )

  (define-test ("Nonterminals have non-unique assignments")
    (collect-errors
     (lambda ()
       (validate-name-assignment! '() (list nonterm21 nonterm22)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:names:ambiguous (car errors)) ) ) )

  (define-test ("Crossing non-unique assignments")
    (collect-errors
     (lambda ()
       (validate-name-assignment! (list term1 term3) (list nonterm1 nonterm3)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:names:ambiguous (car errors)) ) ) )

  (define-test ("Multiple non-unique assignments")
    (collect-errors
     (lambda ()
       (validate-name-assignment! (list term1 term21 term22 term3) (list nonterm1 nonterm21 nonterm22 nonterm3)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:names:ambiguous (car errors)) ) ) )
)
(verify-test-case! validation:name-assignment)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (validation:meta-vars-assignment "Validation of meta-variable uniqueness")

  (define term1 (make-terminal-definition 'term1 null? '(a)))
  (define term2 (make-terminal-definition 'term2 null? '(b b)))
  (define term3 (make-terminal-definition 'term3 null? '(maybe c)))
  (define term4 (make-terminal-definition 'term4 null? '()))
  (define term5 (make-terminal-definition 'term5 null? '(a a)))

  (define nonterm1 (make-nonterminal-definition 'nonterm1 '(C) '()))
  (define nonterm2 (make-nonterminal-definition 'nonterm2 '() '()))
  (define nonterm3 (make-nonterminal-definition 'nonterm3 '(C b) '()))

  (define-test ("Empty list")
    (validate-meta-variable-assignment! '() '()) )

  (define-test ("Normal case")
    (validate-meta-variable-assignment! (list term1 term3) (list nonterm1)) )

  (define-test ("No meta-variables")
    (validate-meta-variable-assignment! (list term4) (list nonterm2)) )

  (define-test ("Duplicate meta-variable names")
    (validate-meta-variable-assignment! (list term2) '()) )

  (define-test ("Terminals have non-unique assignments")
    (collect-errors
     (lambda ()
       (validate-meta-variable-assignment! (list term1 term5) '()) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:meta-vars:ambiguous (car errors)) ) ) )

  (define-test ("Nonterminals have non-unique assignments")
    (collect-errors
     (lambda ()
       (validate-meta-variable-assignment! '() (list nonterm1 nonterm3)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:meta-vars:ambiguous (car errors)) ) ) )

  (define-test ("Terminals and Nonterminals have non-unique assignments")
    (collect-errors
     (lambda ()
       (validate-meta-variable-assignment! (list term2) (list nonterm3)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:meta-vars:ambiguous (car errors)) ) ) )

  (define-test ("Multiple non-unique assignments")
    (collect-errors
     (lambda ()
       (validate-meta-variable-assignment! (list term1 term2 term5) (list nonterm1 nonterm3)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:meta-vars:ambiguous (car errors)) ) ) )
)
(verify-test-case! validation:meta-vars-assignment)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (validation:production-duplicates "Validation of productions (duplicates)")

  (define terminals
   (list (make-terminal-definition 'term1 null? '(a b c))
         (make-terminal-definition 'term2 null? '(d)) ) )

  (define nonterminals
   (list (make-nonterminal-definition 'nonterm1 '(E F) '())
         (make-nonterminal-definition 'nonterm2 '(G)   '())
         (make-nonterminal-definition 'nonterm3 '(H I) '()) ) )

  (define mapping (make-meta-var-mapping terminals nonterminals))

  (define (validate! productions)
    (validate-production-duplicates! mapping productions) )

  (define-test ("Empty list")
    (validate! '()) )

  (define-test ("Normal case")
    (validate! '(a d (G H)) ) )

  (define-test ("Explicit duplicates")
    (collect-errors
     (lambda ()
       (validate! '(a a)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:duplicate (car errors)) ) ) )

  (define-test ("Explicit duplicates (with keyword)")
    (collect-errors
     (lambda ()
       (validate! '((let ((a b)) (E (F G)))
                    (let ((a b)) (E (F G)))) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:duplicate (car errors)) ) ) )

  (define-test ("Explicit duplicates (with suffixes)")
    (collect-errors
     (lambda ()
       (validate! '(a+ a*)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:duplicate (car errors)) ) ) )

  (define-test ("Synonym duplicates")
    (collect-errors
     (lambda ()
       (validate! '(a c)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:duplicate (car errors)) ) ) )

  (define-test ("Synonym duplicates (with keyword)")
    (collect-errors
     (lambda ()
       (validate! '((let ((a b)) (E (F G)))
                    (let ((c a)) (F (E G)))) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:duplicate (car errors)) ) ) )

  (define-test ("Synonym duplicates (with suffixes)")
    (collect-errors
     (lambda ()
       (validate! '((let ((a1 b+)) (E?  (F G^)))
                    (let ((a2 c+)) (F** (E G)))) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:duplicate (car errors)) ) ) )

  (define-test ("Empty list duplicates")
    (collect-errors
     (lambda ()
       (validate! '(() ())) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:duplicate (car errors)) ) ) )
)
(verify-test-case! validation:production-duplicates)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (validation:production-syntax "Validation of productions (syntax)")

  (define invalid-productions (map list `(() 42 "str" ,null? #\N #f #(name))))
  (define invalid-dotted-ends (map list `(ee 42 "str" ,null? #\N #f #(name))))

  (define terminals
   (list (make-terminal-definition 'term1 null? '(a b c))
         (make-terminal-definition 'term2 null? '(d)) ) )

  (define nonterminals
   (list (make-nonterminal-definition 'nonterm1 '(E F) '())
         (make-nonterminal-definition 'nonterm2 '(G)   '())
         (make-nonterminal-definition 'nonterm3 '(H I) '()) ) )

  (define mapping (make-meta-var-mapping terminals nonterminals))

  (define (validate! production)
    (validate-production-syntax! mapping production) )

  (define-test ("Singular terminal meta-variable")
    (validate! 'd) )

  (define-test ("Singular nonterminal meta-variable")
    (validate! 'G) )

  (define-test ("List: simple")
    (validate! '(a b E I)) )

  (define-test ("List: simple with suffixes")
    (validate! '(a1 b2 E3 I4)) )

  (define-test ("List: with maybe")
    (validate! '((maybe a?) b (maybe c?) d)) )

  (define-test ("List: with ...")
    (validate! '(a* ... b H)) )

  (define-test ("List: with !..")
    (validate! '((a+ b+) !..)) )

  (define-test ("Keyword alone")
    (validate! '(null)) )

  (define-test ("Keyworded list")
    (validate! '(let a b c)) )

  (define-test ("Keyworded maybe")
    (validate! '(let (maybe a?))) )

  (define-test ("Keyworded ...")
    (validate! '(let (G G) ...)) )

  (define-test ("Keyworded !..")
    (validate! '(let d !..)) )

  (define-test ("Invalid productions" production)
    #(invalid-productions)
    (collect-errors
     (lambda ()
       (validate! production) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:syntax:invalid (car errors)) ) ) )

  (define-test ("Unknown meta-variable reference")
    (collect-errors
     (lambda ()
       (validate! 'foo) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:syntax:unknown (car errors)) ) ) )

  (define-test ("Unknown meta-variable reference (2)")
    (collect-errors
     (lambda ()
       (validate! '(a b c foo bar)) )
     (lambda (errors)
       (assert-= 2 (length errors))
       (assert-lang-error 'validate:productions:syntax:unknown (car errors))
       (assert-lang-error 'validate:productions:syntax:unknown (cadr errors)) ) ) )

  (define-test ("Repeating nothing: ...")
    (collect-errors
     (lambda ()
       (validate! '(... a?)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:syntax:repeat (car errors)) ) ) )

  (define-test ("Repeating nothing: !..")
    (collect-errors
     (lambda ()
       (validate! '(!.. a?)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:syntax:repeat (car errors)) ) ) )

  (define-test ("Repeating keyword: ...")
    (collect-errors
     (lambda ()
       (validate! '(let ...)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:syntax:repeat (car errors)) ) ) )

  (define-test ("Repeating keyword: !..")
    (collect-errors
     (lambda ()
       (validate! '(let !..)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:syntax:repeat (car errors)) ) ) )

  (define-test ("maybe: empty")
    (collect-errors
     (lambda ()
       (validate! '((maybe))) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:syntax:maybe (car errors)) ) ) )

  (define-test ("maybe: unknown reference")
    (collect-errors
     (lambda ()
       (validate! '((maybe foo))) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:syntax:unknown (car errors)) ) ) )

  (define-test ("maybe: invalid syntax (1)")
    (collect-errors
     (lambda ()
       (validate! '((maybe x? y?))) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:syntax:maybe (car errors)) ) ) )

  (define-test ("maybe: invalid syntax (2)" dot)
    #(invalid-dotted-ends)
    (collect-errors
     (lambda ()
       (validate! `((maybe . ,dot))) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:syntax:maybe (car errors)) ) ) )

  (define-test ("Dotted list: simple" dot)
    #(invalid-dotted-ends)
    (collect-errors
     (lambda ()
       (validate! `(a b . ,dot)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:syntax:invalid (car errors)) ) ) )

  (define-test ("Dotted list: keyword" dot)
    #(invalid-dotted-ends)
    (collect-errors
     (lambda ()
       (validate! `(kw . ,dot)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:syntax:invalid (car errors)) ) ) )

  (define-test ("Dotted list: ..." dot)
    #(invalid-dotted-ends)
    (collect-errors
     (lambda ()
       (validate! `(a b ... . ,dot)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:syntax:invalid (car errors)) ) ) )

  (define-test ("Dotted list: !.." dot)
    #(invalid-dotted-ends)
    (collect-errors
     (lambda ()
       (validate! `(a b ... . ,dot)) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'validate:productions:syntax:invalid (car errors)) ) ) )

  (define-test ("Altogether")
    (collect-errors
     (lambda ()
       (validate! '(!.. (maybe) foo? . d)) )
     (lambda (errors)
       (assert-= 4 (length errors))
       (assert-lang-error 'validate:productions:syntax:repeat  (list-ref errors 0))
       (assert-lang-error 'validate:productions:syntax:maybe   (list-ref errors 1))
       (assert-lang-error 'validate:productions:syntax:unknown (list-ref errors 2))
       (assert-lang-error 'validate:productions:syntax:invalid (list-ref errors 3)) ) ) )
)
(verify-test-case! validation:production-syntax)

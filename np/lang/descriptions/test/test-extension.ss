(import (scheme base)
        (only (srfi 1) lset=)
        (np lang descriptions definitions)
        (np lang descriptions errors)
        (np lang descriptions extension)
        (np lang descriptions test utils)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define (terminal-definitions-equal? a b)
  (and (eq?       (terminal-name a)           (terminal-name b))
       (eqv?      (terminal-predicate a)      (terminal-predicate b))
       (lset= eq? (terminal-meta-variables a) (terminal-meta-variables b)) ) )

(define (nonterminal-definitions-equal? a b)
  (and (eq?          (nonterminal-name a)                   (nonterminal-name b))
       (lset= eq?    (nonterminal-meta-variables a)         (nonterminal-meta-variables b))
       (lset= equal? (nonterminal-production-definitions a) (nonterminal-production-definitions b)) ) )

(define (assert-term-defs-equal expected actual)
  (assert-true (list? expected))
  (assert-true (list? actual))
  (assert-true (lset= terminal-definitions-equal? expected actual)) )

(define (assert-nonterm-defs-equal expected actual)
  (assert-true (list? expected))
  (assert-true (list? actual))
  (assert-true (lset= nonterminal-definitions-equal? expected actual)) )

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

(define-test-case (extension:terminals:add "Terminal addition")

  (define number  (make-terminal-definition 'number number? '(n)))
  (define number1 (make-terminal-definition 'number number? '(u)))
  (define number2 (make-terminal-definition 'number number? '(m)))
  (define string  (make-terminal-definition 'string string? '(s)))
  (define symbol  (make-terminal-definition 'symbol symbol? '(s)))

  (define-test ("Empty lists")
    (assert-equal '() (append-terminal-definitions '() '())) )

  (define-test ("Normal addition")
    (assert-term-defs-equal (list number string)
     (append-terminal-definitions (list number) (list string))) )

  (define-test ("Adding empty list")
    (assert-term-defs-equal (list number)
     (append-terminal-definitions (list number) '())) )

  (define-test ("Adding to empty list")
    (assert-term-defs-equal (list number)
     (append-terminal-definitions '() (list number))) )

  (define-test ("Does not track meta-variable duplication")
    (assert-term-defs-equal (list symbol string)
     (append-terminal-definitions (list symbol) (list string))) )

  (define-test ("Allows to add multiple of the same name")
    (assert-term-defs-equal (list string number number1)
     (append-terminal-definitions (list string) (list number number1))) )

  (define-test ("Allows to add existing name")
    (assert-term-defs-equal (list string number number1)
     (append-terminal-definitions (list string number) (list number1))) )

  (define-test ("Allows to add multiple of the same existing name")
    (assert-term-defs-equal (list string number number1 number2)
     (append-terminal-definitions (list string number) (list number1 number2))) )

  (define-test ("Does not fail on preexisting duplicates")
    (assert-term-defs-equal (list number1 number2 string)
     (append-terminal-definitions (list number1 number2) (list string))) )
)
(verify-test-case! extension:terminals:add)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (extension:terminals:rem "Terminal removal")

  (define number (make-terminal-definition 'number number? '(n)))
  (define string (make-terminal-definition 'string string? '(s)))
  (define symbol (make-terminal-definition 'symbol symbol? '(s)))

  (define-test ("Empty lists")
    (assert-equal '() (remove-terminal-definitions '() '())) )

  (define-test ("Normal removal")
    (assert-term-defs-equal (list number)
     (remove-terminal-definitions (list number string) '(string))) )

  (define-test ("Removing an empty list")
    (assert-term-defs-equal (list number string)
     (remove-terminal-definitions (list number string) '())) )

  (define-test ("Removing everything")
    (assert-term-defs-equal '()
     (remove-terminal-definitions (list number string symbol) '(string number symbol))) )

  (define-test ("Removal of nonexistent definitions")
    (collect-errors
     (lambda ()
       (assert-term-defs-equal (list number string)
        (remove-terminal-definitions (list number string) '(other)) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:term:remove-missing (car errors)) ) ) )

  (define-test ("Removal of multiple nonexistent definitions")
    (collect-errors
     (lambda ()
       (assert-term-defs-equal (list number string)
        (remove-terminal-definitions (list number string) '(other another)) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:term:remove-missing (car errors)) ) ) )

  (define-test ("Duplicate removals")
    (collect-errors
     (lambda ()
       (assert-term-defs-equal (list string)
        (remove-terminal-definitions (list number string) '(number number)) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:term:remove-duplicate (car errors)) ) ) )

  (define-test ("Duplicate removal of nonexistent definitions")
    (collect-errors
     (lambda ()
       (assert-term-defs-equal (list number)
        (remove-terminal-definitions (list number string) '(other other string)) ) )
     (lambda (errors)
       (assert-= 2 (length errors))
       (assert-lang-error 'ext:term:remove-duplicate (car errors))
       (assert-lang-error 'ext:term:remove-missing (cadr errors)) ) ) )
)
(verify-test-case! extension:terminals:rem)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (extension:terminals:mod "Terminal modification")

  (define number (make-terminal-definition 'number number? '(n)))
  (define string (make-terminal-definition 'string string? '(s)))
  (define symbol (make-terminal-definition 'symbol symbol? '(s)))

  (define number* (make-terminal-definition 'number number? '(n m)))
  (define symbol* (make-terminal-definition 'symbol symbol? '(sym)))

  (define number0 (make-terminal-definition 'number number? '()))
  (define number2 (make-terminal-definition 'number number? '(n n)))
  (define number3 (make-terminal-definition 'number number? '(n n n)))

  (define-test ("Empty lists")
    (assert-equal '() (modify-terminal-definitions '() '())) )

  (define-test ("Normal modification (adding variables)")
    (assert-term-defs-equal (list number* string)
     (modify-terminal-definitions (list number string)
      (list (make-terminal-modification 'number '(m) '())) ) ) )

  (define-test ("Normal modification (removing variables)")
    (assert-term-defs-equal (list number)
     (modify-terminal-definitions (list number*)
      (list (make-terminal-modification 'number '() '(m))) ) ) )

  (define-test ("Normal modification (replacing variables)")
    (assert-term-defs-equal (list number symbol*)
     (modify-terminal-definitions (list number symbol)
      (list (make-terminal-modification 'symbol '(sym) '(s))) ) ) )

  (define-test ("Not modifying anything (empty list)")
    (assert-term-defs-equal (list number string)
     (modify-terminal-definitions (list number string) '())) )

  (define-test ("Not modifying anything (with noops)")
    (assert-term-defs-equal (list number string)
     (modify-terminal-definitions (list number string)
      (list (make-terminal-modification 'number '() '())
            (make-terminal-modification 'string '() '()) ) ) ) )

  (define-test ("Allows to add duplicate variables")
    (assert-term-defs-equal (list number2)
     (modify-terminal-definitions (list number)
      (list (make-terminal-modification 'number '(n) '())) ) ) )

  (define-test ("Allows to add duplicate variables (2)")
    (assert-term-defs-equal (list number3)
     (modify-terminal-definitions (list number)
      (list (make-terminal-modification 'number '(n n) '())) ) ) )

  (define-test ("Allows to remove all variables")
    (assert-term-defs-equal (list number0)
     (modify-terminal-definitions (list number)
      (list (make-terminal-modification 'number '() '(n))) ) ) )

  (define-test ("Allows to remove and re-add a variable")
    (assert-term-defs-equal (list string)
     (modify-terminal-definitions (list string)
      (list (make-terminal-modification 'string '(s) '(s))) ) ) )

  (define-test ("Modification of nonexistent definition")
    (collect-errors
     (lambda ()
       (assert-term-defs-equal (list number string)
        (modify-terminal-definitions (list number string)
         (list (make-terminal-modification 'symbol '(s) '(sym))) ) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:term:modify-missing (car errors)) ) ) )

  (define-test ("Duplicate modification")
    (collect-errors
     (lambda ()
       ; no assumptions about what we get as result in this case
       (modify-terminal-definitions (list number)
        (list (make-terminal-modification 'number '(m) '())
              (make-terminal-modification 'number '() '(n)) ) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:term:modify-duplicate (car errors)) ) ) )

  (define-test ("Removal of nonexistent variables")
    (collect-errors
     (lambda ()
       (assert-term-defs-equal (list number string)
        (modify-terminal-definitions (list number string)
         (list (make-terminal-modification 'number '() '(something))) ) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:term:modify:meta-var-remove-missing (car errors)) ) ) )

  (define-test ("Removal of multiple nonexistent variables")
    (collect-errors
     (lambda ()
       (assert-term-defs-equal (list number string)
        (modify-terminal-definitions (list number string)
         (list (make-terminal-modification 'number '() '(something))
               (make-terminal-modification 'string '() '(different)) ) ) ) )
     (lambda (errors)
       (assert-= 2 (length errors))
       (assert-lang-error 'ext:term:modify:meta-var-remove-missing (car errors))
       (assert-lang-error 'ext:term:modify:meta-var-remove-missing (cadr errors)) ) ) )

  (define-test ("Duplicate removal of variables")
    (collect-errors
     (lambda ()
       (assert-term-defs-equal (list number0 string)
        (modify-terminal-definitions (list number string)
         (list (make-terminal-modification 'number '() '(n n))) ) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:term:modify:meta-var-remove-duplicate (car errors)) ) ) )
)
(verify-test-case! extension:terminals:mod)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (extension:nonterminals:add "Nonterminal addition")

  (define Pair   (make-nonterminal-definition 'Pair '(Pair) '((obj . obj))))
  (define Pair1  (make-nonterminal-definition 'Pair '(Pair) '((car . cdr))))
  (define Pair2  (make-nonterminal-definition 'Pair '(Pair) '((sexp . sexp))))
  (define Number (make-nonterminal-definition 'Number '(N) '(num (/ num num))))
  (define None   (make-nonterminal-definition 'None   '(N) '()))

  (define-test ("Empty lists")
    (assert-equal '() (append-nonterminal-definitions '() '())) )

  (define-test ("Normal addition")
    (assert-nonterm-defs-equal (list Pair Number)
     (append-nonterminal-definitions (list Pair) (list Number))) )

  (define-test ("Adding empty list")
    (assert-nonterm-defs-equal (list Number)
     (append-nonterminal-definitions (list Number) '())) )

  (define-test ("Adding to empty list")
    (assert-nonterm-defs-equal (list Number)
     (append-nonterminal-definitions '() (list Number))) )

  (define-test ("Does not track meta-variable duplication")
    (assert-nonterm-defs-equal (list Number None)
     (append-nonterminal-definitions (list Number) (list None))) )

  (define-test ("Allows to add multiple of the same name")
    (assert-nonterm-defs-equal (list Number Pair Pair1)
     (append-nonterminal-definitions (list Number) (list Pair Pair1))) )

  (define-test ("Allows to add existing name")
    (assert-nonterm-defs-equal (list Number Pair Pair1)
     (append-nonterminal-definitions (list Number Pair) (list Pair1))) )

  (define-test ("Allows to add multiple of the same existing name")
    (assert-nonterm-defs-equal (list Number Pair Pair1 Pair2)
     (append-nonterminal-definitions (list Number Pair) (list Pair1 Pair2))) )

  (define-test ("Does not fail on preexisting duplicates")
    (assert-nonterm-defs-equal (list Pair1 Pair2 Pair)
     (append-nonterminal-definitions (list Pair1 Pair2) (list Pair))) )
)
(verify-test-case! extension:nonterminals:add)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (extension:nonterminals:rem "Nonterminal removal")

  (define Pair   (make-nonterminal-definition 'Pair   '(Pair) '((obj . obj))))
  (define Number (make-nonterminal-definition 'Number '(N) '(num (/ num num))))
  (define None   (make-nonterminal-definition 'None   '(N) '()))

  (define-test ("Empty lists")
    (assert-equal '() (remove-nonterminal-definitions '() '())) )

  (define-test ("Normal removal")
    (assert-nonterm-defs-equal (list Number)
     (remove-nonterminal-definitions (list Number Pair) '(Pair))) )

  (define-test ("Removing an empty list")
    (assert-nonterm-defs-equal (list Number Pair)
     (remove-nonterminal-definitions (list Number Pair) '())) )

  (define-test ("Removing everything")
    (assert-nonterm-defs-equal '()
     (remove-nonterminal-definitions (list Number Pair None) '(Pair Number None))) )

  (define-test ("Removal of nonexistent definitions")
    (collect-errors
     (lambda ()
       (assert-nonterm-defs-equal (list Number Pair)
        (remove-nonterminal-definitions (list Number Pair) '(other)) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:nonterm:remove-missing (car errors)) ) ) )

  (define-test ("Removal of multiple nonexistent definitions")
    (collect-errors
     (lambda ()
       (assert-nonterm-defs-equal (list Number Pair)
        (remove-nonterminal-definitions (list Number Pair) '(Other Another)) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:nonterm:remove-missing (car errors)) ) ) )

  (define-test ("Duplicate removals")
    (collect-errors
     (lambda ()
       (assert-nonterm-defs-equal (list Pair)
        (remove-nonterminal-definitions (list Number Pair) '(Number Number)) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:nonterm:remove-duplicate (car errors)) ) ) )

  (define-test ("Duplicate removal of nonexistent definitions")
    (collect-errors
     (lambda ()
       (assert-nonterm-defs-equal (list Number)
        (remove-nonterminal-definitions (list Number Pair) '(Other Other Pair)) ) )
     (lambda (errors)
       (assert-= 2 (length errors))
       (assert-lang-error 'ext:nonterm:remove-duplicate (car errors))
       (assert-lang-error 'ext:nonterm:remove-missing (cadr errors)) ) ) )
)
(verify-test-case! extension:nonterminals:rem)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (extension:nonterminals:mod-meta-vars "Nonterminal modification (meta-variables)")

  (define Number (make-nonterminal-definition 'Number '(N) '()))
  (define Void   (make-nonterminal-definition 'Void   '(V) '()))

  (define Number* (make-nonterminal-definition 'Number '(N M)  '()))
  (define Void*   (make-nonterminal-definition 'Void   '(Void) '()))

  (define Number0 (make-nonterminal-definition 'Number '()      '()))
  (define Number2 (make-nonterminal-definition 'Number '(N N)   '()))
  (define Number3 (make-nonterminal-definition 'Number '(N N N) '()))

  (define-test ("Empty lists")
    (assert-equal '() (modify-nonterminal-definitions '() '() '())) )

  (define-test ("Normal modification (adding variables)")
    (assert-nonterm-defs-equal (list Number* Void)
     (modify-nonterminal-definitions '() (list Number Void)
      (list (make-nonterminal-modification 'Number '(M) '() '() '())) ) ) )

  (define-test ("Normal modification (removing variables)")
    (assert-nonterm-defs-equal (list Number)
     (modify-nonterminal-definitions '() (list Number*)
      (list (make-nonterminal-modification 'Number '() '(M) '() '())) ) ) )

  (define-test ("Normal modification (replacing variables)")
    (assert-nonterm-defs-equal (list Number Void*)
     (modify-nonterminal-definitions '() (list Number Void)
      (list (make-nonterminal-modification 'Void '(Void) '(V) '() '())) ) ) )

  (define-test ("Not modifying anything (empty list)")
    (assert-nonterm-defs-equal (list Number Void)
     (modify-nonterminal-definitions '() (list Number Void) '())) )

  (define-test ("Not modifying anything (with noops)")
    (assert-nonterm-defs-equal (list Number Void)
     (modify-nonterminal-definitions '() (list Number Void)
      (list (make-nonterminal-modification 'Number '() '() '() '())
            (make-nonterminal-modification 'Void   '() '() '() '()) ) ) ) )

  (define-test ("Allows to add duplicate variables")
    (assert-nonterm-defs-equal (list Number2)
     (modify-nonterminal-definitions '() (list Number)
      (list (make-nonterminal-modification 'Number '(N) '() '() '())) ) ) )

  (define-test ("Allows to add duplicate variables (2)")
    (assert-nonterm-defs-equal (list Number3)
     (modify-nonterminal-definitions '() (list Number)
      (list (make-nonterminal-modification 'Number '(N N) '() '() '())) ) ) )

  (define-test ("Allows to remove all variables")
    (assert-nonterm-defs-equal (list Number0)
     (modify-nonterminal-definitions '() (list Number)
      (list (make-nonterminal-modification 'Number '() '(N) '() '())) ) ) )

  (define-test ("Allows to remove and re-add a variable")
    (assert-nonterm-defs-equal (list Void)
     (modify-nonterminal-definitions '() (list Void)
      (list (make-nonterminal-modification 'Void '(V) '(V) '() '())) ) ) )

  (define-test ("Modification of nonexistent definition")
    (collect-errors
     (lambda ()
       (assert-nonterm-defs-equal (list Number Void)
        (modify-nonterminal-definitions '() (list Number Void)
         (list (make-nonterminal-modification 'String '(Str) '(S) '() '())) ) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:nonterm:modify-missing (car errors)) ) ) )

  (define-test ("Duplicate modification")
    (collect-errors
     (lambda ()
       ; no assumptions about what we get as result in this case
       (modify-nonterminal-definitions '() (list Number)
        (list (make-nonterminal-modification 'Number '(Num) '() '() '())
              (make-nonterminal-modification 'Number '() '(N) '() '()) ) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:nonterm:modify-duplicate (car errors)) ) ) )

  (define-test ("Removal of nonexistent variables")
    (collect-errors
     (lambda ()
       (assert-nonterm-defs-equal (list Number Void)
        (modify-nonterminal-definitions '() (list Number Void)
         (list (make-nonterminal-modification 'Number '() '(something) '() '())) ) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:nonterm:modify:meta-var-remove-missing (car errors)) ) ) )

  (define-test ("Removal of multiple nonexistent variables")
    (collect-errors
     (lambda ()
       (assert-nonterm-defs-equal (list Number Void)
        (modify-nonterminal-definitions '() (list Number Void)
         (list (make-nonterminal-modification 'Number '() '(something) '() '())
               (make-nonterminal-modification 'Void '() '(different) '() '()) ) ) ) )
     (lambda (errors)
       (assert-= 2 (length errors))
       (assert-lang-error 'ext:nonterm:modify:meta-var-remove-missing (car errors))
       (assert-lang-error 'ext:nonterm:modify:meta-var-remove-missing (cadr errors)) ) ) )

  (define-test ("Duplicate removal of variables")
    (collect-errors
     (lambda ()
       (assert-nonterm-defs-equal (list Number0 Void)
        (modify-nonterminal-definitions '() (list Number Void)
         (list (make-nonterminal-modification 'Number '() '(N N) '() '())) ) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:nonterm:modify:meta-var-remove-duplicate (car errors)) ) ) )
)
(verify-test-case! extension:nonterminals:mod-meta-vars)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (extension:nonterminals:mod-productions "Nonterminal modification (productions)")

  (define integer (make-terminal-definition 'integer integer? '(i j k)))
  (define string  (make-terminal-definition 'string  string?  '(s)))

  (define Number (make-nonterminal-definition 'Number '(N) '(i (i i) (/ i i))))
  (define Void   (make-nonterminal-definition 'Void   '(V) '(())))
  (define Object (make-nonterminal-definition 'Object '(O) '(N V)))

  (define Number1 (make-nonterminal-definition 'Number '(N) '(i (i i) (/ i i) (k k k))))
  (define Number2 (make-nonterminal-definition 'Number '(N) '(i (/ i i))))
  (define Number3 (make-nonterminal-definition 'Number '(N) '(i (i i) (/ i i) (j j))))
  (define Number4 (make-nonterminal-definition 'Number '(N) '(i (i i) (/ i i) (i i))))

  (define Void0   (make-nonterminal-definition 'Void   '(V) '()))
  (define Void1   (make-nonterminal-definition 'Void   '(V) '(V)))

  (define Object1 (make-nonterminal-definition 'Object '(O) '(N V s)))
  (define Object2 (make-nonterminal-definition 'Object '(O) '(N V s (i) (j) (k))))

  (define-test ("Empty lists")
    (assert-equal '() (modify-nonterminal-definitions '() '() '())) )

  (define-test ("Normal modification (adding productions)")
    (assert-nonterm-defs-equal (list Number1 Void)
     (modify-nonterminal-definitions (list integer string) (list Number Void)
      (list (make-nonterminal-modification 'Number '() '() '((k k k)) '())) ) ) )

  (define-test ("Normal modification (removing productions)")
    (assert-nonterm-defs-equal (list Number2)
     (modify-nonterminal-definitions (list integer string) (list Number)
      (list (make-nonterminal-modification 'Number '() '() '() '((i i)))) ) ) )

  (define-test ("Normal modification (replacing productions)")
    (assert-nonterm-defs-equal (list Number Void1)
     (modify-nonterminal-definitions (list integer string) (list Number Void)
      (list (make-nonterminal-modification 'Void '() '() '(V) '(()))) ) ) )

  (define-test ("Not modifying anything (empty list)")
    (assert-nonterm-defs-equal (list Number Void)
     (modify-nonterminal-definitions (list integer string) (list Number Void) '())) )

  (define-test ("Not modifying anything (with noops)")
    (assert-nonterm-defs-equal (list Number Void)
     (modify-nonterminal-definitions (list integer string) (list Number Void)
      (list (make-nonterminal-modification 'Number '() '() '() '())
            (make-nonterminal-modification 'Void   '() '() '() '()) ) ) ) )

  (define-test ("Allows to add duplicate productions")
    (assert-nonterm-defs-equal (list Number4)
     (modify-nonterminal-definitions (list integer string) (list Number)
      (list (make-nonterminal-modification 'Number '() '() '((i i)) '())) ) ) )

  (define-test ("Allows to add duplicate productions (2)")
    (assert-nonterm-defs-equal (list Number3)
     (modify-nonterminal-definitions (list integer string) (list Number)
      (list (make-nonterminal-modification 'Number '() '() '((j j)) '())) ) ) )

  (define-test ("Allows to add duplicate productions (3)")
    (assert-nonterm-defs-equal (list Object2)
     (modify-nonterminal-definitions (list integer string) (list Object)
      (list (make-nonterminal-modification 'Object '() '() '(s (i) (j) (k)) '())) ) ) )

  (define-test ("Removes by semantic unification")
    (assert-nonterm-defs-equal (list Number2)
     (modify-nonterminal-definitions (list integer string) (list Number)
      (list (make-nonterminal-modification 'Number '() '() '() '((k k)))) ) ) )

  (define-test ("Allows to remove all productions")
    (assert-nonterm-defs-equal (list Void0)
     (modify-nonterminal-definitions (list integer string) (list Void)
      (list (make-nonterminal-modification 'Void '() '() '() '(()))) ) ) )

  (define-test ("Allows to remove and re-add a production")
    (assert-nonterm-defs-equal (list Void)
     (modify-nonterminal-definitions (list integer string) (list Void)
      (list (make-nonterminal-modification 'Void '() '() '(()) '(()))) ) ) )

  (define-test ("Allows to remove and re-add a production (2)")
    (assert-nonterm-defs-equal (list Number4)
     (modify-nonterminal-definitions (list integer string) (list Number3)
      (list (make-nonterminal-modification 'Number '() '() '((i i)) '((j j)))) ) ) )

  (define-test ("Modification of nonexistent definition")
    (collect-errors
     (lambda ()
       (assert-nonterm-defs-equal (list Number Void)
        (modify-nonterminal-definitions (list integer string) (list Number Void)
         (list (make-nonterminal-modification 'String '() '() '(s) '((s s)))) ) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:nonterm:modify-missing (car errors)) ) ) )

  (define-test ("Duplicate modification")
    (collect-errors
     (lambda ()
       ; no assumptions about what we get as result in this case
       (modify-nonterminal-definitions (list integer string) (list Number)
        (list (make-nonterminal-modification 'Number '() '() '((j j)) '())
              (make-nonterminal-modification 'Number '() '() '() '((k k))) ) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:nonterm:modify-duplicate (car errors)) ) ) )

  (define-test ("Removal of nonexistent productions")
    (collect-errors
     (lambda ()
       (assert-nonterm-defs-equal (list Number Void)
        (modify-nonterminal-definitions (list integer string) (list Number Void)
         (list (make-nonterminal-modification 'Number '() '() '() '(something))) ) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:nonterm:modify:production-remove-missing (car errors)) ) ) )

  (define-test ("Removal of nonexistent productions (as literals)")
    (collect-errors
     (lambda ()
       (assert-nonterm-defs-equal (list Number Void)
        (modify-nonterminal-definitions (list string) (list Number Void)
         (list (make-nonterminal-modification 'Number '() '() '() '((j j)))) ) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:nonterm:modify:production-remove-missing (car errors)) ) ) )

  (define-test ("Removal of multiple nonexistent productions")
    (collect-errors
     (lambda ()
       (assert-nonterm-defs-equal (list Number Void)
        (modify-nonterminal-definitions (list integer string) (list Number Void)
         (list (make-nonterminal-modification 'Number '() '() '() '(something))
               (make-nonterminal-modification 'Void   '() '() '() '(different)) ) ) ) )
     (lambda (errors)
       (assert-= 2 (length errors))
       (assert-lang-error 'ext:nonterm:modify:production-remove-missing (car errors))
       (assert-lang-error 'ext:nonterm:modify:production-remove-missing (cadr errors)) ) ) )

  (define-test ("Duplicate removal of production")
    (collect-errors
     (lambda ()
       (assert-nonterm-defs-equal (list Number2 Void)
        (modify-nonterminal-definitions (list integer string) (list Number Void)
         (list (make-nonterminal-modification 'Number '() '() '() '((i i) (i i)))) ) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:nonterm:modify:production-remove-duplicate (car errors)) ) ) )

  (define-test ("Duplicate removal of production (2)")
    (collect-errors
     (lambda ()
       (assert-nonterm-defs-equal (list Number2 Void)
        (modify-nonterminal-definitions (list integer string) (list Number Void)
         (list (make-nonterminal-modification 'Number '() '() '() '((i i) (j j)))) ) ) )
     (lambda (errors)
       (assert-= 1 (length errors))
       (assert-lang-error 'ext:nonterm:modify:production-remove-duplicate (car errors)) ) ) )
)
(verify-test-case! extension:nonterminals:mod-productions)

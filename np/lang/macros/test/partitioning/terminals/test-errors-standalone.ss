(import (scheme base)
        (np lang macros partitioning)
        (np lang macros test utils)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:standalone:invalid-syntax "Partitioning of standalone terminal forms: invalid syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the terminal" lang a-symbol)
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '(a-symbol) ) )) ) )

  (define-test ("clause")
    (assert-syntax-error ("Invalid syntax of the terminal" lang (number number? (n nn) x))
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((number number? (n nn) x)) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Invalid syntax of the terminal" lang ())
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '(()) ) )) ) )

  (define-test ("irregular list 1")
    (assert-syntax-error ("Invalid syntax of the terminal" lang (car . cdr))
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((car . cdr)) ) )) ) )

  (define-test ("irregular list 2")
    (assert-syntax-error ("Invalid syntax of the terminal" lang (a b . c))
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((a b . c)) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the terminal" lang #(symbol? (s)))
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '(#(symbol? (s))) ) )) ) )
)
(verify-test-case! terminals:standalone:invalid-syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:standalone:meta-var-names "Partitioning of standalone terminal forms: meta-variable names")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num #t)
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((Num number? (#t))) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num #\x1234)
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((Num number? (#\x1234))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang number? ())
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((number? (()))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang num (car . cdr))
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((num (car . cdr) ((car . cdr)))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang number (+ x))
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((number number? ((+ x)))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num -6)
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((Num number? (-6))) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num "then")
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((Num number? ("then"))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num #(x))
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((Num number? (#(x)))) ) )) ) )
)
(verify-test-case! terminals:standalone:meta-var-names)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:standalone:meta-var-syntax "Partitioning of standalone terminal forms: meta-variable syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the terminal" lang (number? foo))
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((number? foo)) ) )) ) )

  (define-test ("empty")
    (assert-syntax-error ("At least one meta-variable should be specified for a terminal" lang number?)
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((number? ())) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the terminal" lang (number number? (a b . c)))
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((number number? (a b . c))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the terminal" lang (number? #(1 2 3)))
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((number? #(1 2 3))) ) )) ) )
)
(verify-test-case! terminals:standalone:meta-var-syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:standalone:name "Partitioning of standalone terminal forms: terminal name")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang (#t predicate? (some vars)) #t)
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((#t predicate? (some vars))) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang (#\c predicate? (some vars)) #\c)
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((#\c predicate? (some vars))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang (() p? (some vars)) ())
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((() p? (some vars))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang ((car . cdr) predicate? (some vars)) (car . cdr))
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '(((car . cdr) predicate? (some vars))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang ((name) predicate? (some vars)) (name))
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '(((name) predicate? (some vars))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang (1.0 predicate? (some vars)) 1.0)
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((1.0 predicate? (some vars))) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang ("at" predicate? (some vars)) "at")
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '(("at" predicate? (some vars))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang (#() predicate? (some vars)) #())
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((#() predicate? (some vars))) ) )) ) )
)
(verify-test-case! terminals:standalone:name)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:standalone:predicate "Partitioning of standalone terminal forms: short form predicate syntax")

  (define-test ("empty list")
    (assert-syntax-error ("Terminal predicate must be a variable in short form" lang (() (some vars)) ())
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((() (some vars))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Terminal predicate must be a variable in short form" lang ((a . d) (some vars)) (a . d))
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '(((a . d) (some vars))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Terminal predicate must be a variable in short form" lang ((lambda (x) (odd? x)) (some vars)) (lambda (x) (odd? x)))
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '(((lambda (x) (odd? x)) (some vars))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Terminal predicate must be a variable in short form" lang (#(1 2 3) (some vars)) #(1 2 3))
      ($ ($quote
        ($filter-standalone-terminal-definitions 'lang
          '((#(1 2 3) (some vars))) ) )) ) )
)
(verify-test-case! terminals:standalone:predicate)

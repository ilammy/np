(import (scheme base)
        (np lang macros partitioning)
        (np lang macros test-utils)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:predicate:invalid "Toplevel forms: predicate clause invalid syntax")

  (define-test ("invalid 1")
    (assert-syntax-error ("Invalid syntax of the predicate clause" lang (predicate . 123))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((predicate . 123)) ) )) ) )

  (define-test ("invalid 2")
    (assert-syntax-error ("Invalid syntax of the predicate clause" lang (predicate . #()))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((predicate . #())) ) )) ) )

  (define-test ("invalid 3")
    (assert-syntax-error ("Invalid syntax of the predicate clause" lang (predicate some dotted . list))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((predicate some dotted . list)) ) )) ) )
)
(verify-test-case! toplevel:predicate:invalid)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:predicate:name "Toplevel forms: predicate clause binding name")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the language predicate must be an identifier" lang (predicate #f) #f)
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((predicate #f)) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the language predicate must be an identifier" lang (predicate #\d) #\d)
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((predicate #\d)) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the language predicate must be an identifier" lang (predicate ()) ())
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((predicate ())) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the language predicate must be an identifier" lang (predicate (a . d)) (a . d))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((predicate (a . d))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the language predicate must be an identifier" lang (predicate (1 2 3)) (1 2 3))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((predicate (1 2 3))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the language predicate must be an identifier" lang (predicate 2) 2)
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((predicate 2)) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the language predicate must be an identifier" lang (predicate "walls") "walls")
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((predicate "walls")) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the language predicate must be an identifier" lang (predicate #(some-pred)) #(some-pred))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((predicate #(some-pred))) ) )) ) )
)
(verify-test-case! toplevel:predicate:name)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:predicate:syntax "Toplevel forms: predicate clause syntax errors")

  (define-test ("multiple names")
    (assert-syntax-error ("Only one language predicate name can be specified" lang (predicate 1 2 3))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((predicate 1 2 3)) ) )) ) )

  (define-test ("no name")
    (assert-syntax-error ("Name of the language predicate cannot be empty" lang (predicate))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((predicate)) ) )) ) )

  (define-test ("repeated")
    (assert-syntax-error ("Only one 'predicate' clause can be specified" lang (predicate lang?_1) (predicate lang?_2))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((predicate lang?_1) (predicate lang?_2)) ) )) ) )
)
(verify-test-case! toplevel:predicate:syntax)

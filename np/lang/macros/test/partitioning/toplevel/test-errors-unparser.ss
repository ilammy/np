(import (scheme base)
        (np lang macros partitioning-toplevel)
        (np lang macros test-utils)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:unparser:invalid "Toplevel forms: unparser clause invalid syntax")

  (define-test ("invalid 1")
    (assert-syntax-error ("Invalid syntax of the unparser clause" lang (unparser . 123))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((unparser . 123)) ) )) ) )

  (define-test ("invalid 2")
    (assert-syntax-error ("Invalid syntax of the unparser clause" lang (unparser . #()))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((unparser . #())) ) )) ) )

  (define-test ("invalid 3")
    (assert-syntax-error ("Invalid syntax of the unparser clause" lang (unparser some dotted . list))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((unparser some dotted . list)) ) )) ) )
)
(verify-test-case! toplevel:unparser:invalid)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:unparser:name "Toplevel forms: unparser clause binding name")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the language unparser must be an identifier" lang (unparser #t) #t)
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((unparser #t)) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the language unparser must be an identifier" lang (unparser #\u) #\u)
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((unparser #\u)) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the language unparser must be an identifier" lang (unparser ()) ())
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((unparser ())) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the language unparser must be an identifier" lang (unparser (a . d)) (a . d))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((unparser (a . d))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the language unparser must be an identifier" lang (unparser (1 2 3)) (1 2 3))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((unparser (1 2 3))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the language unparser must be an identifier" lang (unparser 9) 9)
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((unparser 9)) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the language unparser must be an identifier" lang (unparser "break") "break")
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((unparser "break")) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the language unparser must be an identifier" lang (unparser #(some-name)) #(some-name))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((unparser #(some-name))) ) )) ) )
)
(verify-test-case! toplevel:unparser:name)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:unparser:syntax "Toplevel forms: unparser clause syntax errors")

  (define-test ("multiple names")
    (assert-syntax-error ("Only one language unparser name can be specified" lang (unparser 1 2 3))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((unparser 1 2 3)) ) )) ) )

  (define-test ("no name")
    (assert-syntax-error ("Name of the language unparser cannot be empty" lang (unparser))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((unparser)) ) )) ) )

  (define-test ("repeated")
    (assert-syntax-error ("Only one 'unparser' clause can be specified" lang (unparser unparse-lang) (unparser unparse-lang))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((unparser unparse-lang) (parser parse-lang) (unparser unparse-lang)) ) )) ) )
)
(verify-test-case! toplevel:unparser:syntax)

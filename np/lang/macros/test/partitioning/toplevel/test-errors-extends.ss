(import (scheme base)
        (np lang macros partitioning)
        (np lang macros test utils)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:extends:invalid "Toplevel forms: extends clause invalid syntax")

  (define-test ("invalid 1")
    (assert-syntax-error ("Invalid syntax of the extension clause" lang (extends . 123))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((extends . 123)) ) )) ) )

  (define-test ("invalid 2")
    (assert-syntax-error ("Invalid syntax of the extension clause" lang (extends . #()))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((extends . #())) ) )) ) )

  (define-test ("invalid 3")
    (assert-syntax-error ("Invalid syntax of the extension clause" lang (extends some dotted . list))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((extends some dotted . list)) ) )) ) )
)
(verify-test-case! toplevel:extends:invalid)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:extends:name "Toplevel forms: extends clause language name")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the language to be extended must be an identifier" lang (extends #f) #f)
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((extends #f)) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the language to be extended must be an identifier" lang (extends #\:) #\:)
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((extends #\:)) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the language to be extended must be an identifier" lang (extends ()) ())
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((extends ())) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the language to be extended must be an identifier" lang (extends (a . d)) (a . d))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((extends (a . d))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the language to be extended must be an identifier" lang (extends (1 2 3)) (1 2 3))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((extends (1 2 3))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the language to be extended must be an identifier" lang (extends 42) 42)
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((extends 42)) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the language to be extended must be an identifier" lang (extends "last") "last")
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((extends "last")) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the language to be extended must be an identifier" lang (extends #(some-lang)) #(some-lang))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((extends #(some-lang))) ) )) ) )
)
(verify-test-case! toplevel:extends:name)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:extends:syntax "Toplevel forms: extends clause syntax errors")

  (define-test ("multiple names")
    (assert-syntax-error ("Only one language can be extended" lang (extends 1 2 3))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((extends 1 2 3)) ) )) ) )

  (define-test ("no name")
    (assert-syntax-error ("Name of the language to be extended cannot be empty" lang (extends))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((extends)) ) )) ) )

  (define-test ("repeated")
    (assert-syntax-error ("Only one 'extends' clause can be specified" lang (extends some-lang) (extends some-other-lang))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((extends some-lang) (extends some-other-lang)) ) )) ) )
)
(verify-test-case! toplevel:extends:syntax)

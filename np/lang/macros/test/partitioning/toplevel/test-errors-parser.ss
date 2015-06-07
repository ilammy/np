(import (scheme base)
        (np lang macros partitioning)
        (np lang macros test utils)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:parser:invalid "Toplevel forms: parser clause invalid syntax")

  (define-test ("invalid 1")
    (assert-syntax-error ("Invalid syntax of the parser clause" lang (parser . 123))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((parser . 123)) ) )) ) )


  (define-test ("invalid 2")
    (assert-syntax-error ("Invalid syntax of the parser clause" lang (parser . #()))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((parser . #())) ) )) ) )


  (define-test ("invalid 3")
    (assert-syntax-error ("Invalid syntax of the parser clause" lang (parser some dotted . list))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((parser some dotted . list)) ) )) ) )

)
(verify-test-case! toplevel:parser:invalid)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:parser:name "Toplevel forms: parser clause binding name")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the language parser must be an identifier" lang (parser #t) #t)
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((parser #t)) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the language parser must be an identifier" lang (parser #\!) #\!)
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((parser #\!)) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the language parser must be an identifier" lang (parser ()) ())
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((parser ())) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the language parser must be an identifier" lang (parser (a . d)) (a . d))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((parser (a . d))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the language parser must be an identifier" lang (parser (1 2 3)) (1 2 3))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((parser (1 2 3))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the language parser must be an identifier" lang (parser 1) 1)
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((parser 1)) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the language parser must be an identifier" lang (parser "Prison") "Prison")
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((parser "Prison")) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the language parser must be an identifier" lang (parser #(some-parser)) #(some-parser))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((parser #(some-parser))) ) )) ) )
)
(verify-test-case! toplevel:parser:name)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:parser:syntax "Toplevel forms: parser clause syntax errors")

  (define-test ("multiple names")
    (assert-syntax-error ("Only one language parser name can be specified" lang (parser 1 2 3))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((parser 1 2 3)) ) )) ) )


  (define-test ("no name")
    (assert-syntax-error ("Name of the language parser cannot be empty" lang (parser))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((parser)) ) )) ) )


  (define-test ("repeated")
    (assert-syntax-error ("Only one 'parser' clause can be specified" lang (parser parse-lang) (parser parse-lang) (parser parse-lang))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((parser parse-lang) (parser parse-lang) (parser parse-lang)) ) )) ) )
)
(verify-test-case! toplevel:parser:syntax)

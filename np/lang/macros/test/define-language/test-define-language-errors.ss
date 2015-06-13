(import (scheme base)
        (np lang macros define-language)
        (np lang descriptions language)
        (np lang macros test utils)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (test:define-language:syntax "define-language syntax errors")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the toplevel clause" lang bar)
      (define-language lang bar) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the toplevel clause" lang #(bar))
      (define-language lang #(bar)) ) )

  (define-test ("empty")
    (assert-syntax-error ("Invalid syntax of the language definition")
      (define-language) ) )

  (define-test ("irregular list 1")
    (assert-syntax-error ("Invalid syntax of the language definition")
      (define-language . haha!) ) )

  (define-test ("irregular list 2")
    (assert-syntax-error ("Invalid syntax of the language definition")
      (define-language foo . #(bar)) ) )
)
(verify-test-case! test:define-language:syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (test:define-language:name "define-language language names")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the language must be an identifier" #f)
      (define-language #f) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the language must be an identifier" #\space)
      (define-language #\space) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the language must be an identifier" ())
      (define-language ()) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the language must be an identifier" (foo . bar))
      (define-language (foo . bar)) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the language must be an identifier" (foo))
      (define-language (foo)) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the language must be an identifier" 9)
      (define-language 9) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the language must be an identifier" "lang")
      (define-language "lang") ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the language must be an identifier" #(foo))
      (define-language #(foo)) ) )
)
(verify-test-case! test:define-language:name)

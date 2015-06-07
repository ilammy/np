(import (scheme base)
        (np lang macros partitioning)
        (np lang macros test utils)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:terminals "Toplevel forms: terminals clause syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the terminals clause" lang (terminals . some-atom))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((terminals . some-atom)) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the terminals clause" lang (terminals (num number? (n)) . some-atom))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((terminals (num number? (n)) . some-atom)) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the terminals clause" lang (terminals . #(some vector)))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((terminals . #(some vector))) ) )) ) )
)
(verify-test-case! toplevel:terminals)

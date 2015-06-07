(import (scheme base)
        (np lang macros partitioning)
        (np lang macros test utils)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:invalid "Toplevel forms: invalid syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the toplevel clause" lang 42)
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '(42) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the toplevel clause" lang #(obviosly incorrect syntax))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '(#(obviosly incorrect syntax)) ) )) ) )
)
(verify-test-case! toplevel:invalid)

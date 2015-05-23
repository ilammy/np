(import (scheme base)
        (np lang macros partitioning-terminals)
        (np lang macros test-utils)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension:invalid-syntax "Partitioning of extension terminal forms: invalid syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the terminal extension" lang foo)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '(foo) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Invalid syntax of the terminal extension" lang ())
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '(()) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the terminal extension" lang (nil car . cdr))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((nil car . cdr)) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Invalid syntax of the terminal extension" lang (some thing))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((some thing)) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the terminal extension" lang #(3 14 15))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '(#(3 14 15)) ) )) ) )
)
(verify-test-case! terminals:extension:invalid-syntax)

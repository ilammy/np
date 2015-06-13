(import (scheme base)
        (np lang macros partitioning)
        (np lang macros test utils)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension:invalid-syntax "Partitioning of extension nonterminal forms: invalid syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the nonterminal extension" lang foo)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '(foo) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Invalid syntax of the nonterminal extension" lang ())
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '(()) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the nonterminal extension" lang (nil car . cdr))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((nil car . cdr)) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Invalid syntax of the nonterminal extension" lang (some thing))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((some thing)) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the nonterminal extension" lang #(3 14 15))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '(#(3 14 15)) ) )) ) )
)
(verify-test-case! nonterminals:extension:invalid-syntax)

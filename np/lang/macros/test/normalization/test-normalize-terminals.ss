(import (scheme base)
        (np lang macros normalization)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:standalone "Normalization of standalone terminal forms")

  (define-test ("Full form is not transformed")
    (assert-equal '(number number? (n))
      ($ ($quote ($normalize-standalone-terminal-definition
        '(number number? (n)) ))) ) )

  (define-test ("Short form: predicate name duplication")
    (assert-equal '(number? number? (n nn))
      ($ ($quote ($normalize-standalone-terminal-definition
        '(number? (n nn)) ))) ) )
)
(verify-test-case! terminals:standalone)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-addition "Normalization of extension addition terminal forms")

  (define-test ("Full form is not transformed")
    (assert-equal '(number number? (n))
      ($ ($quote ($normalize-extension-terminal-addition
        '(number number? (n)) ))) ) )

  (define-test ("Short form: predicate name duplication")
    (assert-equal '(number? number? (n nn))
      ($ ($quote ($normalize-extension-terminal-addition
        '(number? (n nn)) ))) ) )
)
(verify-test-case! terminals:extension-addition)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-removal "Normalization of extension removal terminal forms")

  (define-test ("Identifier form is not transformed")
    (assert-equal 'number
      ($ ($quote ($normalize-extension-terminal-removal
        'number ))) ) )

  (define-test ("Full form: only name left")
    (assert-equal 'example
      ($ ($quote ($normalize-extension-terminal-removal
        '(example (lambda (x) (odd? x)) (ee)) ))) ) )

  (define-test ("Short form: only name left")
    (assert-equal 'number?
      ($ ($quote ($normalize-extension-terminal-removal
        '(number? (n nn)) ))) ) )
)
(verify-test-case! terminals:extension-removal)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-modification "Normalization of extension modification terminal forms")

  (define-test ("Meta-variable modifications splicing")
    (assert-equal '(number (a b c) (d))
      ($ ($quote ($normalize-extension-terminal-modification
        '(number ((a b c) (d))) ))) ) )

  (define-test ("Empty lists do not cause issues")
    (assert-equal '(haha () ())
      ($ ($quote ($normalize-extension-terminal-modification
        '(haha (() ())) ))) ) )
)
(verify-test-case! terminals:extension-modification)

(import (scheme base)
        (np lang macros define-language)
        (np lang descriptions language)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (test:define-language "Smoke tests of the interface macro")

  (define (assert-defined-language language)
    (assert-true (language-description? language)) )

  (define (assert-defined-procedure procedure)
    (assert-true (procedure? procedure)) )

  (define-test ("Empty language")
    (define-language Foo)
    (assert-defined-language Foo) )

  (define-test ("Standlone language")
    (define-language Foo
      (predicate Foo?)
      (parser parse-Foo)
      (unparser unparse-Foo)

      (terminals
        (number number? (n i j k))
        (boolean boolean? (b)) )

      (Pair Pair? (Pair)
        (n b) (n n) (b n) (b b) ) )

    (assert-defined-language Foo)
    (assert-defined-procedure Foo?)
    (assert-defined-procedure parse-Foo)
    (assert-defined-procedure unparse-Foo)
    (assert-defined-procedure Pair?) )

  (define-test ("Extension language")
    (define-language Foo
      (Empty (e)
        () ) )

    (define-language Bar
      (extends Foo)

      (terminals
        (+ (number? (n))) )

      (! (Empty Empty? ((+ g)))) )

    (assert-defined-language Foo)
    (assert-defined-language Bar) )
)
(verify-test-case! test:define-language)

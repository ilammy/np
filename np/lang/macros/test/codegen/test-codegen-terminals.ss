(import (scheme base)
        (only (srfi 1) every first second)
        (np lang macros codegen)
        (np lang descriptions definitions)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:standalone "Code generation for standalone terminal forms")

  (define-syntax for
    (syntax-rules ()
      ((_ defs normalized-forms . body)
       (let ((defs ($ ($generate-standalone-terminal-definitions normalized-forms))))
         . body )) ) )

  (define (assert-list-of n list)
    (assert-true (list? list))
    (assert-= n (length list))
    (assert-true (every terminal-definition? list)) )

  (define (assert-def def name predicate meta-variables)
    (assert-eq    name           (terminal-name def))
    (assert-eqv   predicate      (terminal-predicate def))
    (assert-equal meta-variables (terminal-meta-variables def)) )

  (define-test ("Empty list")
    (for defs '()
      (assert-list-of 0 defs) ) )

  (define-test ("Smoke test")
    (for defs '((number number? (n)))
      (assert-list-of 1 defs)
      (assert-def (first defs) 'number number? '(n)) ) )

  (define-test ("Does not change meta-variable order")
    (for defs '((number number? (a b c)))
      (assert-list-of 1 defs)
      (assert-def (first defs) 'number number? '(a b c)) ) )

  (define-test ("Can handle several terminals and does not change their order")
    (for defs '((number number? (a b c)) (pair? pair? (p)))
      (assert-list-of 2 defs)
      (assert-def (first defs) 'number number? '(a b c))
      (assert-def (second defs) 'pair? pair? '(p)) ) )
)
(verify-test-case! terminals:standalone)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-addition "Code generation for extension addition terminal forms")

  (define-syntax for
    (syntax-rules ()
      ((_ defs normalized-forms . body)
       (let ((defs ($ ($generate-extension-terminal-additions normalized-forms))))
         . body )) ) )

  (define (assert-list-of n list)
    (assert-true (list? list))
    (assert-= n (length list))
    (assert-true (every terminal-definition? list)) )

  (define (assert-def def name predicate meta-variables)
    (assert-eq    name           (terminal-name def))
    (assert-eqv   predicate      (terminal-predicate def))
    (assert-equal meta-variables (terminal-meta-variables def)) )

  (define-test ("Empty list")
    (for defs '()
      (assert-list-of 0 defs) ) )

  (define-test ("Smoke test")
    (for defs '((number number? (n)))
      (assert-list-of 1 defs)
      (assert-def (first defs) 'number number? '(n)) ) )

  (define-test ("Does not change meta-variable order")
    (for defs '((number number? (a b c)))
      (assert-list-of 1 defs)
      (assert-def (first defs) 'number number? '(a b c)) ) )

  (define-test ("Can handle several terminals and does not change their order")
    (for defs '((number number? (a b c)) (pair? pair? (p)))
      (assert-list-of 2 defs)
      (assert-def (first defs) 'number number? '(a b c))
      (assert-def (second defs) 'pair? pair? '(p)) ) )
)
(verify-test-case! terminals:extension-addition)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-removal "Code generation for extension removal terminal forms")

  (define-syntax for
    (syntax-rules ()
      ((_ rems normalized-forms . body)
       (let ((rems ($ ($generate-extension-terminal-removals normalized-forms))))
         . body )) ) )

  (define (assert-list-of n list)
    (assert-true (list? list))
    (assert-= n (length list))
    (assert-true (every symbol? list)) )

  (define (assert-rem removed name)
    (assert-eq name removed) )

  (define-test ("Empty list")
    (for rems '()
      (assert-list-of 0 rems) ) )

  (define-test ("Smoke test")
    (for rems '(number)
      (assert-list-of 1 rems)
      (assert-rem (first rems) 'number) ) )

  (define-test ("Does not change removed terminal order")
    (for rems '(number pair)
      (assert-list-of 2 rems)
      (assert-rem (first rems) 'number)
      (assert-rem (second rems) 'pair) ) )
)
(verify-test-case! terminals:extension-removal)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-modification "Code generation for extension modification terminal forms")

  (define-syntax for
    (syntax-rules ()
      ((_ mods normalized-forms . body)
       (let ((mods ($ ($generate-extension-terminal-modifications normalized-forms))))
         . body )) ) )

  (define (assert-list-of n list)
    (assert-true (list? list))
    (assert-= n (length list))
    (assert-true (every terminal-modification? list)) )

  (define (assert-mod modified name added-meta-vars removed-meta-vars)
    (assert-eq    name              (modified-terminal-name modified))
    (assert-equal added-meta-vars   (modified-terminal-added-meta-variables modified))
    (assert-equal removed-meta-vars (modified-terminal-removed-meta-variables modified)) )

  (define-test ("Empty list")
    (for mods '()
      (assert-list-of 0 mods) ) )

  (define-test ("Smoke test")
    (for mods '((number () ()))
      (assert-list-of 1 mods)
      (assert-mod (first mods) 'number '() '()) ) )

  (define-test ("Does not change meta-variable order")
    (for mods '((number (a b) (c d)))
      (assert-list-of 1 mods)
      (assert-mod (first mods) 'number '(a b) '(c d)) ) )

  (define-test ("Does not change terminal order")
    (for mods '((number (a) ()) (pair () (p)))
      (assert-list-of 2 mods)
      (assert-mod (first mods) 'number '(a) '())
      (assert-mod (second mods) 'pair '() '(p)) ) )
)
(verify-test-case! terminals:extension-modification)

(import (scheme base)
        (np lang descriptions unification)
        (np lang descriptions definitions)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (unification:meta-var-names "Trimming of meta variable names")

  (define-test ("Smoke test")
    (assert-eq 'test (trim-meta-var-name 'test)) )

  (define-test ("Strips arbitrary decimal number suffixes" before after)
    #('((foo123 foo) (bar5 bar) (zog9999 zog)))
    (assert-eq after (trim-meta-var-name before)) )

  (define-test ("Strips arbitrary combinations of special symbols" before after)
    #('((zush*** zush) (wee+ wee) (omg? omg) (aaaaa^^^^ aaaaa)
        (boom*?*+*?^*^^^ boom) (a+*?+*+*+?*?+*+*+++*++**++ a)))
    (assert-eq after (trim-meta-var-name before)) )

  (define-test ("Strips arbitrary decimal numbers followed by arbitrary combinations of special symbols" before after)
    #('((a1*^^ a) (booo0+++ booo) (woo1231**??^ woo)))
    (assert-eq after (trim-meta-var-name before)) )

  (define-test ("Strips only the last combinations")
    (assert-eq 'check123*+* (trim-meta-var-name 'check123*+*123))
    (assert-eq 'check*+*    (trim-meta-var-name 'check*+*123*+*)) )

  (define-test ("Clears entirely special symbols" name)
    #('((+) (?) (^) (*) (+++++***) (*?+^*?+^) (|123+?*|) (|1|) (|1?|)))
    (assert-eq '|| (trim-meta-var-name name)) )

  (define-test ("Tricky cases" before after)
    #('((|+123| +) (*1* *) (*?1 *?) (|| ||)))
    (assert-eq after (trim-meta-var-name before)) )
)
(verify-test-case! unification:meta-var-names)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (unification:productions "Production comparison")

  (define mapping (make-meta-var-mapping
   (list (make-terminal-definition 'number number? '(n m))
         (make-terminal-definition 'string string? '(s str)) )
   (list (make-nonterminal-definition 'Number '(Num Number) '())
         (make-nonterminal-definition 'NamedNumber '(Name NamedNumber) '()) ) ))

  (define-test ("Normal case")
    (assert-true
     (productions-equal? mapping '(Num n s) '(Num n s)) ) )

  (define-test ("Empty lists are equal")
    (assert-true
     (productions-equal? mapping '() '()) ) )

  (define-test ("Can use any of meta-vars")
    (assert-true
     (productions-equal? mapping '(Num n s) '(Number m str)) ) )

  (define-test ("Suffixes do not matter for meta-vars")
    (assert-true
     (productions-equal? mapping '(Num* n+ s9) '(Number000 m? str???*)) ) )

  (define-test ("Can handle literals")
    (assert-true
     (productions-equal? mapping '(/ Num Num) '(/ Number Number)) ) )

  (define-test ("Can handle maybe")
    (assert-true
     (productions-equal? mapping '(+ (maybe Num) Num) '(+ (maybe Number) Number)) ) )

  (define-test ("Can handle repetitions")
    (assert-true
     (productions-equal? mapping '(n ... s !..) '(m ... str !..)) ) )

  (define-test ("Suffixes do matter for literals")
    (assert-false
     (productions-equal? mapping '(e n n s) '(e* n* n* s*)) ) )

  (define-test ("Literals do not match meta-vars")
    (assert-false
     (productions-equal? mapping 'n 'N) ) )

  (define-test ("Different entities do not match")
    (assert-false
     (productions-equal? mapping 'Num 'n) ) )
)
(verify-test-case! unification:productions)

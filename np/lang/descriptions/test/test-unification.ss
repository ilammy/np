(import (scheme base)
        (np lang descriptions unification)
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

(import (scheme base)
        (np lang macros partitioning)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:standalone "Filtering of standalone terminal forms")

  (define-test ("is not afraid of empty list")
    (assert-equal '()
      ($ ($quote ($filter-standalone-terminal-definitions 'lang '()))) ) )

  (define-test ("accepts full form")
    (assert-equal '((number number? (n)))
      ($ ($quote ($filter-standalone-terminal-definitions 'lang
        '((number number? (n))) ))) ) )

  (define-test ("accepts expressions in full form")
    (assert-equal '((odd-number (lambda (x) (and (number? x) (odd? x))) (o-n)))
      ($ ($quote ($filter-standalone-terminal-definitions 'lang
        '((odd-number (lambda (x) (and (number? x) (odd? x))) (o-n))) ))) ) )

  (define-test ("leaves clauses in order")
    (assert-equal '((terminal1 pred1 (v11 v12 v13))
                    (terminal2 pred2 (v21 v22 v23))
                    (terminal3 pred3 (v31 v32 v33)))
      ($ ($quote ($filter-standalone-terminal-definitions 'lang
        '((terminal1 pred1 (v11 v12 v13))
          (terminal2 pred2 (v21 v22 v23))
          (terminal3 pred3 (v31 v32 v33))) ))) ) )

  (define-test ("accepts peculiar extension-like forms")
    (assert-equal '((+ (Look like (extension forms))
                    (while in fact)) (- (they are) (not)))
      ($ ($quote ($filter-standalone-terminal-definitions 'lang
        '((+ (Look like (extension forms)) (while in fact))
          (- (they are) (not))) ))) ) )
)
(verify-test-case! terminals:standalone)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-addition "Partitioning of extension addition terminal forms")

  (define-test ("recognizes addition forms")
    (assert-equal '(((number number? (n)) (symbol symbol? (s))) () ())
      ($ ($quote ($partition-extension-terminal-definitions 'lang
        '((+ (number number? (n)))
          (+ (symbol symbol? (s)))) ))) ) )

  (define-test ("recognizes addition forms with multiple definitions")
    (assert-equal '(((number number? (n)) (symbol symbol? (s))) () ())
      ($ ($quote ($partition-extension-terminal-definitions 'lang
        '((+ (number number? (n))
             (symbol symbol? (s)) )) ))) ) )
)
(verify-test-case! terminals:extension-addition)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-removal "Partitioning of extension removal terminal forms")

  (define-test ("recognizes full removal forms")
    (assert-equal '(() ((void (lambda (x) (odd? x)) (v))) ())
      ($ ($quote ($partition-extension-terminal-definitions 'lang
        '((- (void (lambda (x) (odd? x)) (v)))) ))) ) )

  (define-test ("recognizes full removal forms with multiple definitions")
    (assert-equal '(() ((void (lambda (x) (odd? x)) (v)) (term term? (t tt))) ())
      ($ ($quote ($partition-extension-terminal-definitions 'lang
        '((- (void (lambda (x) (odd? x)) (v))
             (term term? (t tt)))) ))) ) )

  (define-test ("recognizes short removal forms")
    (assert-equal '(() (some removed terminals) ())
      ($ ($quote ($partition-extension-terminal-definitions 'lang
        '((- some removed) (- terminals)) ))) ) )

  (define-test ("recognizes mixed removal forms")
    (assert-equal '(() (some removed (terminal terminal? (t))) ())
      ($ ($quote ($partition-extension-terminal-definitions 'lang
        '((- some removed (terminal terminal? (t)))) ))) ) )
)
(verify-test-case! terminals:extension-removal)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-modification "Partitioning of extension modification terminal forms")

  (define-test ("recognizes meta-var addition")
    (assert-equal '(() () ((some-term ((n) ()))))
      ($ ($quote ($partition-extension-terminal-definitions 'lang
        '((! (some-term ((+ n))))) ))) ) )

  (define-test ("recognizes meta-var removal")
    (assert-equal '(() () ((some-term (() (m)))))
      ($ ($quote ($partition-extension-terminal-definitions 'lang
        '((! (some-term ((- m))))) ))) ) )

  (define-test ("groups modified meta-vars")
    (assert-equal '(() () ((some-term ((c d e g h) (a b f)))))
      ($ ($quote ($partition-extension-terminal-definitions 'lang
        '((! (some-term ((- a b) (+ c d e) (- f) (+ g h))))) ))) ) )

  (define-test ("recognizes multiple modifications")
    (assert-equal '(() () ((foo ((foo) ())) (bar (() (bar))) (baz ((baz) ()))))
      ($ ($quote ($partition-extension-terminal-definitions 'lang
        '((! (foo ((+ foo)))
             (bar ((- bar))))
          (! (baz ((+ baz))))) ))) ) )
)
(verify-test-case! terminals:extension-modification)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-peculiar "Partitioning of tricky extension terminal forms")

  (define-test ("is not afraid of empty list")
    (assert-equal '(() () ())
      ($ ($quote ($partition-extension-terminal-definitions 'lang '()))) ) )

  (define-test ("can handle all forms altogether")
    (assert-equal '(((tar var? (x)) (x x (x)))
                    (some removed (term terminal? (t)))
                    ((zog (() (var)))))
      ($ ($quote ($partition-extension-terminal-definitions 'lang
        '((- some removed) (+ (tar var? (x))) (- (term terminal? (t)))
          (! (zog ((- var)))) (+ (x x (x)))) ))) ) )

  (define-test ("has not restrictions on terminal naming")
    (assert-equal '(((+ + (+))) ((- - (-))) ((! ((+) (-)))))
      ($ ($quote ($partition-extension-terminal-definitions 'lang
        '((+ (+ + (+))) (- (- - (-))) (! (! ((+ +) (- -))))) ))) ) )
)
(verify-test-case! terminals:extension-peculiar)

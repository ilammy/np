(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:standalone "Filtering of standalone terminal forms")

  (define-test ("is not afraid of empty list")
    (assert-equal '()
      ($ ($quote ($filter-standalone-terminal-descriptions 'lang '()))) ) )

  (define-test ("accepts full form")
    (assert-equal '((number number? (n)))
      ($ ($quote ($filter-standalone-terminal-descriptions 'lang
        '((number number? (n))) ))) ) )

  (define-test ("accepts short form")
    (assert-equal '((number? (n nn)))
      ($ ($quote ($filter-standalone-terminal-descriptions 'lang
        '((number? (n nn))) ))) ) )

  (define-test ("accepts expressions in full form")
    (assert-equal '((odd-number (lambda (x) (and (number? x) (odd? x))) (o-n)))
      ($ ($quote ($filter-standalone-terminal-descriptions 'lang
        '((odd-number (lambda (x) (and (number? x) (odd? x))) (o-n))) ))) ) )

  (define-test ("leaves clauses in order")
    (assert-equal '((terminal1 (v11 v12 v13))
                    (terminal2 (v21 v22 v23))
                    (terminal3 (v31 v32 v33)))
      ($ ($quote ($filter-standalone-terminal-descriptions 'lang
        '((terminal1 (v11 v12 v13))
          (terminal2 (v21 v22 v23))
          (terminal3 (v31 v32 v33))) ))) ) )

  (define-test ("accepts peculiar extension-like forms")
    (assert-equal '((+ (Look like (extension forms))
                    (while in fact)) (- (they are not)))
      ($ ($quote ($filter-standalone-terminal-descriptions 'lang
        '((+ (Look like (extension forms)) (while in fact))
          (- (they are not))) ))) ) )
)
(verify-test-case! terminals:standalone)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-addition "Partitioning of extension addition terminal forms")

  (define-test ("recognizes implicit addition forms")
    (assert-equal '(((number number? (n)) (symbol? (s))) () ())
      ($ ($quote ($partition-extension-terminal-descriptions 'lang
        '((number number? (n)) (symbol? (s))) ))) ) )

  (define-test ("recognizes explicit addition forms")
    (assert-equal '(((number? (n)) (symbol symbol? (s))) () ())
      ($ ($quote ($partition-extension-terminal-descriptions 'lang
        '((+ (number? (n)))
          (+ (symbol symbol? (s)))) ))) ) )

  (define-test ("recognizes explicit addition forms with multiple descriptions")
    (assert-equal '(((number? (n)) (symbol? (s))) () ())
      ($ ($quote ($partition-extension-terminal-descriptions 'lang
        '((+ (number? (n))
             (symbol? (s)) )) ))) ) )

  ;; Technical detail, putting a test here to avoid getting sudden and
  ;; unexplainable test failures if the order changes in the future.
  (define-test ("explicit additions appear before implicit")
    (assert-equal '(((a (a)) (b (b)) (c (c)) (d (d)) (e (e))) () ())
      ($ ($quote ($partition-extension-terminal-descriptions 'lang
        '((d (d)) (+ (a (a)) (b (b))) (e (e)) (+ (c (c)))) ))) ) )
)
(verify-test-case! terminals:extension-addition)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-removal "Partitioning of extension removal terminal forms")

  (define-test ("recognizes full removal forms")
    (assert-equal '(() ((void (lambda (x) (odd? x)) (v))) ())
      ($ ($quote ($partition-extension-terminal-descriptions 'lang
        '((- (void (lambda (x) (odd? x)) (v)))) ))) ) )

  (define-test ("recognizes full removal forms with multiple descriptions")
    (assert-equal '(() ((void (lambda (x) (odd? x)) (v)) (term? (t tt))) ())
      ($ ($quote ($partition-extension-terminal-descriptions 'lang
        '((- (void (lambda (x) (odd? x)) (v))
             (term? (t tt)))) ))) ) )

  (define-test ("recognizes short removal forms")
    (assert-equal '(() (some removed terminals) ())
      ($ ($quote ($partition-extension-terminal-descriptions 'lang
        '((- some removed) (- terminals)) ))) ) )
)
(verify-test-case! terminals:extension-removal)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-modification "Partitioning of extension modification terminal forms")

  (define-test ("recognizes meta-var addition")
    (assert-equal '(() () ((some-term ((n) ()))))
      ($ ($quote ($partition-extension-terminal-descriptions 'lang
        '((some-term ((+ n)))) ))) ) )

  (define-test ("recognizes meta-var removal")
    (assert-equal '(() () ((some-term (() (m)))))
      ($ ($quote ($partition-extension-terminal-descriptions 'lang
        '((some-term ((- m)))) ))) ) )

  (define-test ("groups modified meta-vars")
    (assert-equal '(() () ((some-term ((c d e g h) (a b f)))))
      ($ ($quote ($partition-extension-terminal-descriptions 'lang
        '((some-term ((- a b) (+ c d e) (- f) (+ g h)))) ))) ) )
)
(verify-test-case! terminals:extension-modification)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-peculiar "Partitioning of tricky extension terminal forms")

  (define-test ("is not afraid of empty list")
    (assert-equal '(() () ())
      ($ ($quote ($partition-extension-terminal-descriptions 'lang '()))) ) )

  (define-test ("can handle all forms altogether")
    (assert-equal '(((x (x)) (y y (y)) (tar var? (x)))
                    (some removed (terminal? (t)))
                    ((zog (() (var)))))
      ($ ($quote ($partition-extension-terminal-descriptions 'lang
        '((- some removed) (tar var? (x)) (- (terminal? (t)))
          (zog ((- var))) (+ (x (x)) (y y (y)))) ))) ) )

  (define-test ("does not mess up in hard-to-tell cases")
    (assert-equal '(((- (some meta vars))
                     (+ (some-var) (x y))
                     (- predicate? (var))
                     (+ (num? (x)) (var x)))
                    ()
                    ((- ((add) (remove)))
                     (+ ((remove) (add)))
                     (- (() (-)))))
      ($ ($quote ($partition-extension-terminal-descriptions 'lang
        '((- (some meta vars))
          (+ (some-var) (x y))
          (- predicate? (var))
          (+ (num? (x)) (var x))
          (- ((+ add) (- remove)))
          (+ ((- add) (+ remove)))
          (- ((- -)))) ))) ) )
)
(verify-test-case! terminals:extension-peculiar)

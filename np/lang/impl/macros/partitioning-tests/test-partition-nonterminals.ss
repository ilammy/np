(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:standalone:forms "Filtering of standalone nonterminal forms")

  (define-test ("is not afraid of empty list")
    (assert-equal '()
      ($ ($quote ($filter-standalone-nonterminal-descriptions 'lang '()))) ) )

  (define-test ("accepts normal form")
    (assert-equal '((Atom (atom) number symbol string boolean))
      ($ ($quote ($filter-standalone-nonterminal-descriptions 'lang
        '((Atom (atom) number symbol string boolean)) ))) ) )

  (define-test ("accepts form with predicate")
    (assert-equal '((Pair Pair? (pair) (value value)))
      ($ ($quote ($filter-standalone-nonterminal-descriptions 'lang
        '((Pair Pair? (pair) (value value))) ))) ) )

  (define-test ("accepts nonterminals without meta-variables")
    (assert-equal '((Pair Pair? () (value value)))
      ($ ($quote ($filter-standalone-nonterminal-descriptions 'lang
        '((Pair Pair? () (value value))) ))) ) )

  (define-test ("accepts peculiar extension-like forms")
    (assert-equal '((Addition () (+ some list)) (Removal () (- (some (other list)))))
      ($ ($quote ($filter-standalone-nonterminal-descriptions 'lang
        '((Addition () (+ some list)) (Removal () (- (some (other list))))) ))) ) )
)
(verify-test-case! nonterminals:standalone:forms)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:productions "Acceptance of nonterminal productions")

  (define-test ("accepts atoms")
    (assert-equal '((Foo () atom))
      ($ ($quote ($filter-standalone-nonterminal-descriptions 'lang
        '((Foo () atom)) ))) ) )

  (define-test ("accepts empty lists")
    (assert-equal '((Foo () ()))
      ($ ($quote ($filter-standalone-nonterminal-descriptions 'lang
        '((Foo () ())) ))) ) )

  (define-test ("accepts proper lists")
    (assert-equal '((Foo () (atom1 atom2 ...)))
      ($ ($quote ($filter-standalone-nonterminal-descriptions 'lang
        '((Foo () (atom1 atom2 ...))) ))) ) )

  (define-test ("accepts dotted lists")
    (assert-equal '((Foo () (car . cdr)))
      ($ ($quote ($filter-standalone-nonterminal-descriptions 'lang
        '((Foo () (car . cdr))) ))) ) )

  (define-test ("accepts all this stuff composed")
    (assert-equal '((Foo () (v1 v2 (v3 () v4 . v5) v6 ... . v8)))
      ($ ($quote ($filter-standalone-nonterminal-descriptions 'lang
        '((Foo () (v1 v2 (v3 () v4 . v5) v6 ... . v8))) ))) ) )
)
(verify-test-case! nonterminals:productions)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-addition "Partitioning of extension addition nonterminal forms")

  (define-test ("recognizes implicit addition forms")
    (assert-equal '(((Pair Pair? () (value value))) () ())
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((Pair Pair? () (value value))) ))) ) )

  (define-test ("can tell the difference between implicit addition and modification by meta-variable list")
    (assert-equal '(((PlusMinus (pm) (+ stuff) (- stuff))) () ())
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((PlusMinus (pm) (+ stuff) (- stuff))) ))) ) )

  (define-test ("can tell the difference between implicit addition and modification by production list")
    (assert-equal '(((Foo () foo) (Bar () (bar)) (Baz Baz? () (* (foo bar)))) () ())
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((Foo () foo) (Bar () (bar)) (Baz Baz? () (* (foo bar)))) ))) ) )

  (define-test ("recognizes explicit addition forms")
    (assert-equal '(((Pair () (v v))) () ())
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((+ (Pair () (v v)))) ))) ) )

  (define-test ("recognizes explicit addition forms with multiple descriptions")
    (assert-equal '(((Foo () foo) (Bar () bar)) () ())
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((+ (Foo () foo) (Bar () bar))) ))) ) )

  (define-test ("recognizes explicit addition forms with extension-like productions")
    (assert-equal '(((PlusMinus () (+ stuff) (- stuff))) () ())
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((+ (PlusMinus () (+ stuff) (- stuff)))) ))) ) )

  ;; Technical detail, putting a test here to avoid getting sudden and
  ;; unexplainable test failures if the order changes in the future.
  (define-test ("explicit additions appear before implicit")
    (assert-equal '(((A () a) (B () b)) () ())
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((B () b) (+ (A () a))) ))) ) )
)
(verify-test-case! terminals:extension-addition)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-removal "Partitioning of extension removal nonterminal forms")

  (define-test ("recognizes full removal forms")
    (assert-equal '(() ((Pair Pair? () (value value))) ())
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((- (Pair Pair? () (value value)))) ))) ) )

  (define-test ("recognizes full removal forms with multiple descriptions")
    (assert-equal '(() ((Pair Pair? () (value value))) ())
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((- (Pair Pair? () (value value)))) ))) ) )

  (define-test ("recognizes full removal forms without productions")
    (assert-equal '(() ((Atom ())) ())
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((- (Atom ()))) ))) ) )

  (define-test ("recognizes short removal forms")
    (assert-equal '(() (Some Removed Nonterminals) ())
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((- Some Removed) (- Nonterminals)) ))) ) )

  (define-test ("recognizes mixed removal forms")
    (assert-equal '(() (Some (Atom ()) (Pair Pair? () (value value))) ())
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((- Some (Atom ()) (Pair Pair? () (value value)))) ))) ) )
)
(verify-test-case! terminals:extension-removal)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-modification "Partitioning of extension modification terminal forms")

  (define-test ("recognizes predicate redefinition")
    (assert-equal '(() () ((Atom NewAtom? (() ()) (() ()))))
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((Atom NewAtom? ())) ))) ) )

  (define-test ("recognizes meta-var addition")
    (assert-equal '(() () ((Atom ((atom) ()) (() ()))))
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((Atom ((+ atom)))) ))) ) )

  (define-test ("recognizes meta-var removal")
    (assert-equal '(() () ((Atom (() (bork)) (() ()))))
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((Atom ((- atom)))) ))) ) )

  (define-test ("groups modified meta-vars")
    (assert-equal '(() () ((Atom ((v1 v4) (v2 v3)) (() ()))))
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((Nt ((+ v1) (- v2 v3) (+ v4)))) ))) ) )

  ;; Note how modification is preferred over implicit addition for
  ;; clauses without specified meta-variables.
  (define-test ("recognizes production addition")
    (assert-equal '(() () ((Foo (() ()) ((n) ()))))
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((Foo () (+ n))) ))) ) )

  (define-test ("recognizes production removal")
    (assert-equal '(() () ((Foo (() ()) (() (m)))))
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((Foo () (- m))) ))) ) )

  (define-test ("groups modified productions")
    (assert-equal '(() () ((Bar (() ()) (((n n)) (m ())))))
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((Bar () (- m) (+ (n n)) (- ()))) ))) ) )

  (define-test ("can handle all options at the same time")
    (assert-equal '(() () ((Mega Predicate? ((mega) (form)) ((prod) ((list ...))))))
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((Mega Predicate? ((+ mega) (- form)) (+ prod) (- (list ...)))) ))) ) )
)
(verify-test-case! terminals:extension-modification)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-peculiar "Partitioning of tricky extension terminal forms")

  (define-test ("is not afraid of empty list")
    (assert-equal '(() () ())
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang '()))) ) )

  (define-test ("can handle all forms altogether")
    (assert-equal '(((Pair () (v v)) (PlusMinus (pm) (+ stuff) (- stuff)))
                    (Some Removed Nonterminals)
                    ((Atom ((atom) ()) (() ()))))
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((+ (Pair () (v v))) (PlusMinus (pm) (+ stuff) (- stuff))
          (- Some Removed) (Atom ((+ atom))) (- Nonterminals)) ))) ) )

  (define-test ("does not mess up in hard-to-tell cases")
    (assert-equal '(((- () (+ +)) (+ () n) (- Minus? (some vars) (+ something)))
                    (Minus? (some (vars)) (and something (else)))
                    ((+ - (() ()) (() ())) (- + ((-) ()) (() ())) (+ (() ()) (() (+)))))
      ($ ($quote ($partition-extension-nonterminal-descriptions 'lang
        '((- + ((+ -)))
          (+ (- () (+ +)))
          (- Minus? (some vars) (+ something))
          (+ - ())
          (- Minus? (some (vars)) (and something (else)))
          (+ () n)
          (+ () (- +))) ))) ) )
)
(verify-test-case! terminals:extension-peculiar)

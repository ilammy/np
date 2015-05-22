(import (scheme base)
        (np lang macros normalization-nonterminals)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:standalone "Normalization of standalone nonterminal forms")

  (define-test ("Regular form: default predicate value and production unsplicing")
    (assert-equal '(Atom #f (atom) (number symbol string boolean))
      ($ ($quote ($normalize-standalone-nonterminal-definition
        '(Atom (atom) number symbol string boolean) ))) ) )

  (define-test ("Form with predicate: predicate splicing and production unsplicing")
    (assert-equal '(Pair Pair? (pair) ((value value)))
      ($ ($quote ($normalize-standalone-nonterminal-definition
        '(Pair Pair? (pair) (value value)) ))) ) )
)
(verify-test-case! nonterminals:standalone)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-addition "Normalization of extension addition nonterminal forms")

  (define-test ("Regular form: default predicate value and production unsplicing")
    (assert-equal '(Atom #f (atom) (number symbol string boolean))
      ($ ($quote ($normalize-extension-nonterminal-addition
        '(Atom (atom) number symbol string boolean) ))) ) )

  (define-test ("Form with predicate: predicate splicing and production unsplicing")
    (assert-equal '(Pair Pair? (pair) ((value value)))
      ($ ($quote ($normalize-extension-nonterminal-addition
        '(Pair Pair? (pair) (value value)) ))) ) )
)
(verify-test-case! nonterminals:extension-addition)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-removal "Normalization of extension removal nonterminal forms")

  (define-test ("Identifier form is not transformed")
    (assert-equal 'Number
      ($ ($quote ($normalize-extension-nonterminal-removal
        'Number ))) ) )

  (define-test ("Regular form: only name left")
    (assert-equal 'Atom
      ($ ($quote ($normalize-extension-nonterminal-removal
        '(Atom (atom) number symbol string boolean) ))) ) )

  (define-test ("Form with predicate: only name left")
    (assert-equal 'Pair
      ($ ($quote ($normalize-extension-nonterminal-removal
        '(Pair Pair? (pair) (value value)) ))) ) )
)
(verify-test-case! nonterminals:extension-removal)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-modification "Normalization of extension modification nonterminal forms")

  (define-test ("Meta-variable modifications splicing")
    (assert-equal '(Nt #f (v1 v2) (v3 v4) (v5) ())
      ($ ($quote ($normalize-extension-nonterminal-modification
        '(Nt ((v1 v2) (v3 v4)) ((v5) ())) ))) ) )

  (define-test ("Empty lists do not cause issues")
    (assert-equal '(Nt #f () () () ())
      ($ ($quote ($normalize-extension-nonterminal-modification
        '(Nt (() ()) (() ())) ))) ) )

  (define-test ("Form with predicate: predicate splicing")
    (assert-equal '(Nt Nt? (x) () (y) ())
      ($ ($quote ($normalize-extension-nonterminal-modification
        '(Nt Nt? ((x) ()) ((y) ())) ))) ) )

  (define-test ("Form with predicate: pure")
    (assert-equal '(Nt Nt? () () () ())
      ($ ($quote ($normalize-extension-nonterminal-modification
        '(Nt Nt? (() ()) (() ())) ))) ) )
)
(verify-test-case! nonterminals:extension-modification)

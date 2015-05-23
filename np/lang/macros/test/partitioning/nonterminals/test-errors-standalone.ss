(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (np lang macros test-utils)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:standalone:invalid-syntax "Partitioning of standalone nonterminal forms: invalid syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang 42)
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '(42) ) )) ) )

  (define-test ("clause 1")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Number))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Number)) ) )) ) )

  (define-test ("clause 2")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Number pred))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Number pred)) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang ())
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '(()) ) )) ) )

  (define-test ("irregular list 1")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Number . HA-HA!))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Number . HA-HA!)) ) )) ) )

  (define-test ("irregular list 2")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Number pred . HA-HA!))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Number pred . HA-HA!)) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang #(Number (num) n))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '(#(Number (num) n)) ) )) ) )
)
(verify-test-case! nonterminals:standalone:invalid-syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:standalone:meta-var-names "Partitioning of standalone nonterminal forms: meta-variable names")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number #t)
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Number (#t) n)) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number #\i)
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Number (#\i) n)) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number ())
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Number (()) n)) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number (a . d))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Number Pred? ((a . d)) n1 n2)) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number (+ x))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Number ((+ x)) n)) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number 49)
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Number (49) n)) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number "in")
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Number ("in") n)) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number #(9))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Number (#(9)) n)) ) )) ) )
)
(verify-test-case! nonterminals:standalone:meta-var-names)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:standalone:meta-var-syntax "Partitioning of standalone nonterminal forms: meta-variable syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Number SURPRISE! n))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Number SURPRISE! n)) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Number (a . d) n))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Number (a . d) n)) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Number a #(b) n))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Number a #(b) n)) ) )) ) )
)
(verify-test-case! nonterminals:standalone:meta-var-syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:standalone:name "Partitioning of standalone nonterminal forms: nonterminal name")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang (#t (some vars)) #t)
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((#t (some vars) production1)) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang (#\j (some vars)) #\j)
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((#\j (some vars) production1)) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang (() (var)) ())
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((() (var) production1 (production2 production3))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang ((car . cdr) pred? ()) (car . cdr))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '(((car . cdr) pred? () production1)) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang ((some list) pred? (some vars)) (some list))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '(((some list) pred? (some vars) production1)) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang (13 (some vars)) 13)
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((13 (some vars) production1)) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang ("the" (some vars)) "the")
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '(("the" (some vars) production1)) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang (#(1) (some vars)) #(1))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((#(1) (some vars) production1)) ) )) ) )
)
(verify-test-case! nonterminals:standalone:name)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:standalone:predicate "Partitioning of standalone nonterminal forms: predicate name")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang (Nonterminal #f (var1 var2)) #f)
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Nonterminal #f (var1 var2) (p p))) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang (Nonterminal #\K (var1 var2)) #\K)
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Nonterminal #\K (var1 var2) (p p))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang (Nonterminal (a . d) ()) (a . d))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Nonterminal (a . d) () "technically incorrect production")) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang (Nonterminal -4 (var1 var2)) -4)
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Nonterminal -4 (var1 var2) (p p))) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang (Nonterminal "night" (var1 var2)) "night")
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Nonterminal "night" (var1 var2) (p p))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang (Nonterminal #(x y z) (var1 var2)) #(x y z))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Nonterminal #(x y z) (var1 var2) (p p))) ) )) ) )
)
(verify-test-case! nonterminals:standalone:predicate)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:standalone:productions:syntax "Partitioning of standalone nonterminal forms: production syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Nonterminal p () . sudden-atom))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Nonterminal p () . sudden-atom)) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("At least one production should be specified for a nonterminal" lang Nonterminal)
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Nonterminal (nt))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Nonterminal () foo . bar))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Nonterminal () foo . bar)) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Nonterminal (nt) . #()))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Nonterminal (nt) . #())) ) )) ) )
)
(verify-test-case! nonterminals:standalone:productions:syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:standalone:productions:vector "Partitioning of standalone nonterminal forms: production no-vector tests")

  (define-test ("nested")
    (assert-syntax-error ("Invalid syntax of the production: vector patterns are not allowed" lang Nonterminal (some (deep list #(123))) #(123))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Nonterminal () foo bar (some (deep list #(123))))) ) )) ) )

  (define-test ("toplevel")
    (assert-syntax-error ("Invalid syntax of the production: vector patterns are not allowed" lang Nonterminal #(vector!))
      ($ ($quote
        ($filter-standalone-nonterminal-definitions 'lang
          '((Nonterminal () foo bar #(vector!))) ) )) ) )
)
(verify-test-case! nonterminals:standalone:productions:vector)

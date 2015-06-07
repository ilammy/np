(import (scheme base)
        (np lang macros partitioning)
        (np lang macros test utils)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-removal:invalid-syntax "Partitioning of extension nonterminal removal forms: invalid nonterminal syntax")

  (define-test ("clause 1")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Number))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Number))) ) )) ) )

  (define-test ("clause 2")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Number pred))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Number pred))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang ())
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- ())) ) )) ) )

  (define-test ("irregular list 1")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Number . HA-HA!))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Number . HA-HA!))) ) )) ) )

  (define-test ("irregular list 2")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Number pred . HA-HA!))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Number pred . HA-HA!))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang #(Number (num) n))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- #(Number (num) n))) ) )) ) )
)
(verify-test-case! nonterminals:extension-removal:invalid-syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-removal:meta-var-names "Partitioning of extension nonterminal removal forms: meta variable names")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number #f)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Number (#f) n))) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number #\null)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Number (#\null) n))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number ())
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Number (()) n))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number (a . d))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Number Pred? ((a . d)) n1 n2))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number (+ x))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Number ((+ x))))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number 3.0)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Number (3.0) n))) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number "on")
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Number ("on") n))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number #(9))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Number (#(9)) n))) ) )) ) )
)
(verify-test-case! nonterminals:extension-removal:meta-var-names)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-removal:meta-var-syntax "Partitioning of extension nonterminal removal forms: meta variable syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Number SURPRISE!))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Number SURPRISE!))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Number (a . d) n))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Number (a . d) n))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Number a #(b) n))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Number a #(b) n))) ) )) ) )
)
(verify-test-case! nonterminals:extension-removal:meta-var-syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-removal:name "Partitioning of extension nonterminal removal forms: nonterminal names")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang (#t (some vars)) #t)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (#t (some vars)))) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang (#\G (some vars)) #\G)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (#\G (some vars)))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang (() (var)) ())
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (() (var)))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang ((car . cdr) pred? ()) (car . cdr))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- ((car . cdr) pred? () production1))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang ((some list) pred? (some vars)) (some list))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- ((some list) pred? (some vars) production1))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang (4 (some vars)) 4)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (4 (some vars)))) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang ("high" (some vars)) "high")
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- ("high" (some vars)))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang (#(1) (some vars)) #(1))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (#(1) (some vars)))) ) )) ) )
)
(verify-test-case! nonterminals:extension-removal:name)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-removal:predicate "Partitioning of extension nonterminal removal forms: predicate names")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang (Nonterminal #f (var1 var2)) #f)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Nonterminal #f (var1 var2) (p p)))) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang (Nonterminal #\h (var1 var2)) #\h)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Nonterminal #\h (var1 var2) (p p)))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang (Nonterminal (a . d) ()) (a . d))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Nonterminal (a . d) () "technically incorrect production"))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang (Nonterminal 12 (var1 var2)) 12)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Nonterminal 12 (var1 var2) (p p)))) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang (Nonterminal "There" (var1 var2)) "There")
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Nonterminal "There" (var1 var2) (p p)))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang (Nonterminal #(x y z) (var1 var2)) #(x y z))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Nonterminal #(x y z) (var1 var2) (p p)))) ) )) ) )
)
(verify-test-case! nonterminals:extension-removal:predicate)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-removal:syntax "Partitioning of extension nonterminal removal forms: extension syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the nonterminal extension" lang (- . right-away))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- . right-away)) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("At least one nonterminal should be specified for removal" lang (-))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((-)) ) )) ) )

  (define-test ("irregular list 1")
    (assert-syntax-error ("Invalid syntax of the nonterminal extension" lang (- (Number (n) n n) . random))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Number (n) n n) . random)) ) )) ) )

  (define-test ("irregular list 2")
    (assert-syntax-error ("Invalid syntax of the nonterminal extension" lang (- some . nonterminals))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- some . nonterminals)) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the nonterminal extension" lang (- . #(Number pp (n))))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- . #(Number pp (n)))) ) )) ) )
)
(verify-test-case! nonterminals:extension-removal:syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-removal:production:syntax "Partitioning of extension nonterminal removal forms: productions syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Nonterminal p () . sudden-atom))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Nonterminal p () . sudden-atom))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Nonterminal () foo . bar))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Nonterminal () foo . bar))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the nonterminal" lang (Nonterminal (nt) . #()))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Nonterminal (nt) . #()))) ) )) ) )
)
(verify-test-case! nonterminals:extension-removal:production:syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-removal:production:vector "Partitioning of extension nonterminal removal forms: productions no-vector tests")

  (define-test ("nested")
    (assert-syntax-error ("Invalid syntax of the production: vector patterns are not allowed" lang Nonterminal (some (deep list #(123))) #(123))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Nonterminal () foo bar (some (deep list #(123)))))) ) )) ) )

  (define-test ("toplevel")
    (assert-syntax-error ("Invalid syntax of the production: vector patterns are not allowed" lang Nonterminal #(vector!))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((- (Nonterminal () foo bar #(vector!)))) ) )) ) )
)
(verify-test-case! nonterminals:extension-removal:production:vector)

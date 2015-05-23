(import (scheme base)
        (np lang macros partitioning)
        (np lang macros test-utils)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-modification:invalid-syntax "Partitioning of extension nonterminal modification forms: invalid nonterminal syntax")

  (define-test ("forbidden no modifications")
    (assert-syntax-error ("Nonterminal modification should modify either meta-variables or productions" lang Nonterminal)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Nonterminal ()))) ) )) ) )

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the nonterminal modification" lang xyzzy)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! xyzzy)) ) )) ) )

  (define-test ("clause")
    (assert-syntax-error ("Invalid syntax of the nonterminal modification" lang (Number))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Number))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Invalid syntax of the nonterminal modification" lang ())
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! ())) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the nonterminal modification" lang (Number . HA-HA!))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Number . HA-HA!))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the nonterminal modification" lang #(Number ((+ x)) n))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! #(Number ((+ x)) n))) ) )) ) )
)
(verify-test-case! nonterminals:extension-modification:invalid-syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-modification:meta-var-addition:names "Partitioning of extension nonterminal modification forms: meta variable addition names")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num #t)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Num ((+ #t))))) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num #\D)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Num ((+ #\D))))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number ())
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Number ((+ ()))))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num (car . cdr))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Num ((+ (car . cdr)))))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number (+ x))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Number Number? ((+ (+ x)))))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num 31)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Num ((+ 31))))) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num "sky")
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Num ((+ "sky"))))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num #(x))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Num ((+ #(x)))))) ) )) ) )
)
(verify-test-case! nonterminals:extension-modification:meta-var-addition:names)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-modification:meta-var-addition:syntax "Partitioning of extension nonterminal modification forms: meta variable addition syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang Foo (+ . foo))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Foo ((+ . foo))))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("At least one meta-variable should be specified for addition" lang Foo (+))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Foo ((+))))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang Foo (+ car . cdr))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Foo ((+ car . cdr))))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang Foo (+ . #()))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Foo ((+ . #()))))) ) )) ) )
)
(verify-test-case! nonterminals:extension-modification:meta-var-addition:syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-modification:meta-var-invalid-syntax "Partitioning of extension nonterminal modification forms: meta variable invalid syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang Pair atom)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Pair Pair? (atom (- x))))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang Pair ())
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Pair ((- x) (+ y) ())))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang Pair (y . z))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Pair ((+ x) (y . z))))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang Pair (random list))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Pair ((- x) (+ y) (random list))))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang Pair #(z))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Pair ((- x) (+ y) #(z))))) ) )) ) )
)
(verify-test-case! nonterminals:extension-modification:meta-var-invalid-syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-modification:meta-var-removal:names "Partitioning of extension nonterminal modification forms: meta variable removal names")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num #f)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Num ((- #f))))) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num #\e)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Num ((- #\e))))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number ())
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Number ((- ()))))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number (car . cdr))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Number ((- (car . cdr)))))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Number (+ x))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Number ((- (+ x)))))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num 0)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Num ((- 0))))) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num "way")
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Num ((- "way"))))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num #(x))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Num ((- #(x)))))) ) )) ) )
)
(verify-test-case! nonterminals:extension-modification:meta-var-removal:names)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-modification:meta-var-removal:syntax "Partitioning of extension nonterminal modification forms: meta variable removal syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang Foo (- . foo))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Foo ((- . foo))))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("At least one meta-variable should be specified for removal" lang Foo (-))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Foo ((-))))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang Foo (- car . cdr))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Foo ((- car . cdr))))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang Foo (- . #()))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Foo ((- . #()))))) ) )) ) )
)
(verify-test-case! nonterminals:extension-modification:meta-var-removal:syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-modification:name "Partitioning of extension nonterminal modification forms: nonterminal names")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang (#t ((+ vars))) #t)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (#t ((+ vars)) (+ prod)))) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang (#\f False? ((+ vars))) #\f)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (#\f False? ((+ vars)) (+ prod)))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang (() ()) ())
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (() ()))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang ((a . d) ((+ x))) (a . d))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! ((a . d) ((+ x)) (+ n)))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang ((awesome name) ()) (awesome name))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! ((awesome name) () (+ x)))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang (9 ((+ vars))) 9)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (9 ((+ vars)) (+ prod)))) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang ("up" ((+ vars))) "up")
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! ("up" ((+ vars)) (+ prod)))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the nonterminal must be an identifier" lang (#(9) ((+ vars))) #(9))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (#(9) ((+ vars)) (+ prod)))) ) )) ) )
)
(verify-test-case! nonterminals:extension-modification:name)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-modification:syntax "Partitioning of extension nonterminal modification forms: extension syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the nonterminal extension" lang (! . right-away))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! . right-away)) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("At least one nonterminal should be specified for modification" lang (!))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((!)) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the nonterminal extension" lang (! (Number ((+ x))) . random))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Number ((+ x))) . random)) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the nonterminal extension" lang (! . #(Number () (+ p))))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! . #(Number () (+ p)))) ) )) ) )
)
(verify-test-case! nonterminals:extension-modification:syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-modification:production-additional:syntax "Partitioning of extension nonterminal modification forms: productions addition syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the production modification" lang Foo (+ . atom))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Foo () (+ . atom)))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("At least one production should be specified for addition" lang Foo (+))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Foo ((+ x)) (+)))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the production modification" lang Foo (+ p . x))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Foo Foo? () (+ p . x)))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the production modification" lang Foo (+ . #()))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Foo () (+ . #())))) ) )) ) )
)
(verify-test-case! nonterminals:extension-modification:production-additional:syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-modification:production-additional:vector "Partitioning of extension nonterminal modification forms: productions addition no-vector tests")

  (define-test ("nested")
    (assert-syntax-error ("Invalid syntax of the production: vector patterns are not allowed" lang Nonterminal (x #(p)) #(p))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Nonterminal () (+ (x #(p)))))) ) )) ) )

  (define-test ("toplevel")
    (assert-syntax-error ("Invalid syntax of the production: vector patterns are not allowed" lang Nonterminal #(p))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Nonterminal () (+ #(p))))) ) )) ) )
)
(verify-test-case! nonterminals:extension-modification:production-additional:vector)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-modification:production-removal:syntax "Partitioning of extension nonterminal modification forms: productions removal syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the production modification" lang Foo (- . atom))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Foo Foo? () (- . atom)))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("At least one production should be specified for removal" lang Foo (-))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Foo ((+ x)) (-)))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the production modification" lang Foo (- p . x))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Foo () (- p . x)))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the production modification" lang Foo (- . #()))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Foo () (- . #())))) ) )) ) )
)
(verify-test-case! nonterminals:extension-modification:production-removal:syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-modification:production-removal:vector "Partitioning of extension nonterminal modification forms: productions removal no-vector tests")

  (define-test ("nested")
    (assert-syntax-error ("Invalid syntax of the production: vector patterns are not allowed" lang Nonterminal (x #(p)) #(p))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Nonterminal Nonterminal? () (- (x #(p)))))) ) )) ) )

  (define-test ("toplevel")
    (assert-syntax-error ("Invalid syntax of the production: vector patterns are not allowed" lang Nonterminal #(p))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Nonterminal () (- #(p))))) ) )) ) )
)
(verify-test-case! nonterminals:extension-modification:production-removal:vector)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-modification:production:syntax "Partitioning of extension nonterminal modification forms: productions extension syntax errors")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the nonterminal modification" lang (Pair ((+ x)) . foo))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Pair ((+ x)) . foo))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the nonterminal modification" lang (Pair ((+ x)) (+ n (n n)) . foo))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Pair ((+ x)) (+ n (n n)) . foo))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the nonterminal modification" lang (Pair ((+ x)) . #(1 2 3)))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Pair ((+ x)) . #(1 2 3)))) ) )) ) )
)
(verify-test-case! nonterminals:extension-modification:production:syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-modification:production:invalid "Partitioning of extension nonterminal modification forms: productions invalid syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the production modification" lang Pair production)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Pair ((+ x)) production))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Invalid syntax of the production modification" lang Pair ())
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Pair Pair? ((+ x)) ()))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the production modification" lang Pair (a . d))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Pair ((- x)) (a . d)))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Invalid syntax of the production modification" lang Pair (x y z))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Pair ((+ x)) (x y z)))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the production modification" lang Pair #(9))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Pair ((+ x)) #(9)))) ) )) ) )
)
(verify-test-case! nonterminals:extension-modification:production:invalid)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-modification:predicate "Partitioning of extension nonterminal modification forms: predicate rebinding")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang Number #f)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Number #f ((+ x))))) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang Number #\N)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Number #\N))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang Nonterminal ((- x) . foo))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Nonterminal ((- x) . foo)))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang Number 42)
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Number 42))) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang Number "gazillion")
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Number "gazillion"))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the nonterminal predicate must be an identifier" lang Number #(9))
      ($ ($quote
        ($partition-extension-nonterminal-definitions 'lang
          '((! (Number #(9) ((+ x))))) ) )) ) )
)
(verify-test-case! nonterminals:extension-modification:predicate)

(import (scheme base)
        (np lang macros partitioning)
        (np lang macros test-utils)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-modification:invalid-syntax "Partitioning of extension terminal modification forms: invalid terminal syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the terminal modification" lang a-symbol)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! a-symbol)) ) )) ) )

  (define-test ("clause 1")
    (assert-syntax-error ("Invalid syntax of the terminal modification" lang (name predicate? ((+ x) (- y))))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (name predicate? ((+ x) (- y))))) ) )) ) )

  (define-test ("clause 2")
    (assert-syntax-error ("Invalid syntax of the terminal modification" lang (number? ((+ x) (- y)) x))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (number? ((+ x) (- y)) x))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Invalid syntax of the terminal modification" lang ())
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! ())) ) )) ) )

  (define-test ("irregular list 1")
    (assert-syntax-error ("Invalid syntax of the terminal modification" lang (car . cdr))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (car . cdr))) ) )) ) )

  (define-test ("irregular list 2")
    (assert-syntax-error ("Invalid syntax of the terminal modification" lang (a b . c))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (a b . c))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the terminal modification" lang #(symbol? ((+ s))))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! #(symbol? ((+ s))))) ) )) ) )
)
(verify-test-case! terminals:extension-modification:invalid-syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-modification:meta-var-addition:names "Partitioning of extension terminal modification forms: meta variable addition names")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num #t)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (Num ((+ #t))))) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num #\X)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (Num ((+ #\X))))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang number? ())
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (number? ((+ ()))))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang num (car . cdr))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (num ((+ (car . cdr)))))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang number? (+ x))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (number? ((+ (+ x)))))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num 11)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (Num ((+ 11))))) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num "now")
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (Num ((+ "now"))))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num #(x))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (Num ((+ #(x)))))) ) )) ) )
)
(verify-test-case! terminals:extension-modification:meta-var-addition:names)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-modification:meta-var-addition:syntax "Partitioning of extension terminal modification forms: meta variable addition syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang number? (+ . foo))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (number? ((+ . foo))))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("At least one meta-variable should be specified for addition" lang number? (+))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (number? ((+))))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang num (+ car . cdr))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (num ((+ car . cdr))))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang number? (+ . #()))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (number? ((+ . #()))))) ) )) ) )
)
(verify-test-case! terminals:extension-modification:meta-var-addition:syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-modification:meta-var-invalid-syntax "Partitioning of extension terminal modification forms: meta variable invalid syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang num atom)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (num (atom (- x))))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang num ())
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (num ((- x) (+ y) ())))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang num (y . z))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (num ((+ x) (y . z))))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang num (random list))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (num ((- x) (+ y) (random list))))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang num #(z))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (num ((- x) (+ y) #(z))))) ) )) ) )
)
(verify-test-case! terminals:extension-modification:meta-var-invalid-syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-modification:meta-var-removal:names "Partitioning of extension terminal modification forms: meta variable removal names")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num #t)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (Num ((- #t))))) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num #\.)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (Num ((- #\.))))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang number? ())
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (number? ((- ()))))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang num (car . cdr))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (num ((- (car . cdr)))))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang number? (+ x))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (number? ((- (+ x)))))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num 222)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (Num ((- 222))))) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num "right")
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (Num ((- "right"))))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang Num #(x))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (Num ((- #(x)))))) ) )) ) )
)
(verify-test-case! terminals:extension-modification:meta-var-removal:names)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-modification:meta-var-removal:syntax "Partitioning of extension terminal modification forms: meta variable removal syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang number? (- . foo))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (number? ((- . foo))))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("At least one meta-variable should be specified for removal" lang number? (-))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (number? ((-))))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang num (- car . cdr))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (num ((- car . cdr))))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the meta-variable modification" lang number? (- . #()))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (number? ((- . #()))))) ) )) ) )
)
(verify-test-case! terminals:extension-modification:meta-var-removal:syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-modification:meta-var-syntax "Partitioning of extension terminal modification forms: meta variable extension syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the terminal modification" lang (foo bar))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (foo bar))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Terminal modification should modify meta-variables" lang foo)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (foo ()))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the terminal modification" lang (num ((- x) . foo)))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (num ((- x) . foo)))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the terminal modification" lang (bar #((+ x y z))))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (bar #((+ x y z))))) ) )) ) )
)
(verify-test-case! terminals:extension-modification:meta-var-syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-modification:name "Partitioning of extension terminal modification forms: terminal names")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang (#t ((+ some) (- vars))) #t)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (#t ((+ some) (- vars))))) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang (#\O ((+ some) (- vars))) #\O)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (#\O ((+ some) (- vars))))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang (() ((+ some) (- vars))) ())
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (() ((+ some) (- vars))))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang ((car . cdr) ((+ some) (- vars))) (car . cdr))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! ((car . cdr) ((+ some) (- vars))))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang ((name) ((+ some) (- vars))) (name))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! ((name) ((+ some) (- vars))))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang (11 ((+ some) (- vars))) 11)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (11 ((+ some) (- vars))))) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang ("Eons" ((+ some) (- vars))) "Eons")
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! ("Eons" ((+ some) (- vars))))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang (#(1) ((+ some) (- vars))) #(1))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (#(1) ((+ some) (- vars))))) ) )) ) )
)
(verify-test-case! terminals:extension-modification:name)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-modification:syntax "Partitioning of extension terminal modification forms: extension syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the terminal extension" lang (! . right-away))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! . right-away)) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("At least one terminal should be specified for modification" lang (!))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((!)) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the terminal extension" lang (! (number? ((+ n))) . random))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! (number? ((+ n))) . random)) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the terminal extension" lang (! . #(number? ((+ n)))))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((! . #(number? ((+ n))))) ) )) ) )
)
(verify-test-case! terminals:extension-modification:syntax)

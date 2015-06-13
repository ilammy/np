(import (scheme base)
        (np lang macros partitioning)
        (np lang macros test utils)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-addition:invalid-syntax "Partitioning of extension terminal addition forms: invalid terminal syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the terminal" lang a-symbol)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ a-symbol)) ) )) ) )

  (define-test ("clause")
    (assert-syntax-error ("Invalid syntax of the terminal" lang (number? (n nn)))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (number? (n nn)))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Invalid syntax of the terminal" lang ())
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ ())) ) )) ) )

  (define-test ("irregular list 1")
    (assert-syntax-error ("Invalid syntax of the terminal" lang (car . cdr))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (car . cdr))) ) )) ) )

  (define-test ("irregular list 2")
    (assert-syntax-error ("Invalid syntax of the terminal" lang (a b . c))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (a b . c))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the terminal" lang #(symbol? (s)))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ #(symbol? (s)))) ) )) ) )
)
(verify-test-case! terminals:extension-addition:invalid-syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-addition:meta-var-names "Partitioning of extension terminal addition forms: meta variable names")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang num #t)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (num number? (#t)))) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang num #\4)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (num number? (#\4)))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang num ())
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (num number? (())))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang num (car . cdr))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (num number? ((car . cdr))))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang num (+ x))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (num number? ((+ x))))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang num 4)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (num number? (4)))) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang num "stars")
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (num number? ("stars")))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the meta-variable must be an identifier" lang num #(x))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (num number? (#(x))))) ) )) ) )
)
(verify-test-case! terminals:extension-addition:meta-var-names)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-addition:meta-var-syntax "Partitioning of extension terminal addition forms: meta variable syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the terminal" lang (num number? foo))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (num number? foo))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("At least one meta-variable should be specified for a terminal" lang num)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (num number? ()))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the terminal" lang (num number? (car . cdr)))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (num number? (car . cdr)))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the terminal" lang (num number? #(1 2 3)))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (num number? #(1 2 3)))) ) )) ) )
)
(verify-test-case! terminals:extension-addition:meta-var-syntax)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-addition:name "Partitioning of extension terminal addition forms: terminal names")

  (define-test ("boolean")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang (#f predicate? (some vars)) #f)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (#f predicate? (some vars)))) ) )) ) )

  (define-test ("char")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang (#\L predicate? (some vars)) #\L)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (#\L predicate? (some vars)))) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang (() predicate? (some vars)) ())
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (() predicate? (some vars)))) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang ((car . cdr) predicate? (some vars)) (car . cdr))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ ((car . cdr) predicate? (some vars)))) ) )) ) )

  (define-test ("list")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang ((name) predicate? (some vars)) (name))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ ((name) predicate? (some vars)))) ) )) ) )

  (define-test ("number")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang (0 predicate? (some vars)) 0)
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (0 predicate? (some vars)))) ) )) ) )

  (define-test ("string")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang ("are" predicate? (some vars)) "are")
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ ("are" predicate? (some vars)))) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Name of the terminal must be an identifier" lang (#() predicate? (some vars)) #())
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (#() predicate? (some vars)))) ) )) ) )
)
(verify-test-case! terminals:extension-addition:name)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:extension-addition:syntax "Partitioning of extension terminal addition forms: extension syntax")

  (define-test ("atom")
    (assert-syntax-error ("Invalid syntax of the terminal extension" lang (+ . right-away))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ . right-away)) ) )) ) )

  (define-test ("empty list")
    (assert-syntax-error ("At least one terminal should be specified for addition" lang (+))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+)) ) )) ) )

  (define-test ("irregular list")
    (assert-syntax-error ("Invalid syntax of the terminal extension" lang (+ (number? (n)) . random))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ (number? (n)) . random)) ) )) ) )

  (define-test ("vector")
    (assert-syntax-error ("Invalid syntax of the terminal extension" lang (+ . #(number? (n))))
      ($ ($quote
        ($partition-extension-terminal-definitions 'lang
          '((+ . #(number? (n)))) ) )) ) )
)
(verify-test-case! terminals:extension-addition:syntax)

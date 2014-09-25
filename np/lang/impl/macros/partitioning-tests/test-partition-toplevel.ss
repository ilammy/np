(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:singular "Partitioning of toplevel forms (singular clauses)")

  (define-test ("default values")
    (assert-equal '(#f #f #f #f () ())
      ($ ($quote
        ($partition-toplevel-clauses 'lang '()) )) ) )

  (define-test ("recognizes 'extends' clause")
    (assert-equal '((another-lang) #f #f #f () ())
      ($ ($quote
        ($partition-toplevel-clauses 'lang '((extends another-lang))) )) ) )

  (define-test ("recognizes 'predicate' clause")
    (assert-equal '(#f (lang?) #f #f () ())
      ($ ($quote
        ($partition-toplevel-clauses 'lang '((predicate lang?))) )) ) )

  (define-test ("recognizes 'parser' clause")
    (assert-equal '(#f #f (parse-lang) #f () ())
      ($ ($quote
        ($partition-toplevel-clauses 'lang '((parser parse-lang))) )) ) )

  (define-test ("recognizes 'unparser' clause")
    (assert-equal '(#f #f #f (unparse-lang) () ())
      ($ ($quote
        ($partition-toplevel-clauses 'lang '((unparser unparse-lang))) )) ) )

  (define-test ("handles all singular clauses altogether")
    (assert-equal '((another-lang) (lang?) (parse-lang) (unparse-lang) () ())
      ($ ($quote
        ($partition-toplevel-clauses 'lang '((predicate lang?)
                                             (extends another-lang)
                                             (unparser unparse-lang)
                                             (parser parse-lang))) )) ) )
)
(verify-test-case! toplevel:singular)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:terminals "Partitioning of toplevel forms (terminals clauses)")

  (define-test ("recognizes 'terminals' clause")
    (assert-equal '(#f #f #f #f ((num number? (n))) ())
      ($ ($quote
        ($partition-toplevel-clauses 'lang '((terminals (num number? (n))))) )) ) )

  (define-test ("accepts empty 'terminals' clauses")
    (assert-equal '(#f #f #f #f () ())
      ($ ($quote
        ($partition-toplevel-clauses 'lang '((terminals) (terminals))) )) ) )

  (define-test ("squashes 'terminals' contents")
    (assert-equal '(#f #f #f #f ((num number? (n)) (sym symbol? (s)) (null? (x))) ())
      ($ ($quote
        ($partition-toplevel-clauses 'lang '((terminals (num number? (n))
                                                        (sym symbol? (s)))
                                             (terminals)
                                             (terminals (null? (x))))) )) ) )

  (define-test ("recognizes extension 'terminals' clause")
    (assert-equal '(#f #f #f #f ((+ (num number? (n))
                                    (null? (x)) )
                                 (void void? ((+ vv)))
                                 (- some other terms)) ())
      ($ ($quote
        ($partition-toplevel-clauses 'lang '((terminals (+ (num number? (n))
                                                           (null? (x)) )
                                                        (void void? ((+ vv)))
                                                        (- some other terms) ))) )) ) )
)
(verify-test-case! toplevel:terminals)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:nonterminals "Partitioning of toplevel forms (nonterminal clauses)")

  (define-test ("recognizes normal nonterminal clause")
    (assert-equal '(#f #f #f #f () ((Pair Pair? (p) (x x))))
      ($ ($quote
        ($partition-toplevel-clauses 'lang '((Pair Pair? (p) (x x)))) )) ) )

  (define-test ("recognizes extension nonterminal clause")
    (assert-equal '(#f #f #f #f () ((+ (Num-Pair (np) (n n)))
                                    (- Some Non Terminals)
                                    (Mod-Stuff ((- uncool) (+ cool)))))
      ($ ($quote
        ($partition-toplevel-clauses 'lang '((+ (Num-Pair (np) (n n)))
                                             (- Some Non Terminals)
                                             (Mod-Stuff ((- uncool) (+ cool))))) )) ) )

  (define-test ("keeps nonterminal clauses in order")
    (assert-equal '(#f #f #f #f ((num number? (n))) ((+ (Num-Pair (np) (n n)))
                                                     (Mod-Stuff ((- uncool) (+ cool)))
                                                     (- Some Non Terminals)))
      ($ ($quote
        ($partition-toplevel-clauses 'lang '((+ (Num-Pair (np) (n n)))
                                             (terminals (num number? (n)))
                                             (Mod-Stuff ((- uncool) (+ cool)))
                                             (- Some Non Terminals))) )) ) )
)
(verify-test-case! toplevel:nonterminals)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:peculiar "Partitioning of toplevel forms (peculiar cases)")

  (define-test ("clause order is (mostly) irrelevant")
    (assert-equal '((another-lang) (lang?) (parse-lang) #f
                    ((null? (x))) ((Pair Pair? (p) (x x))
                                   (- Some Non Terminals)
                                   (+ (Num-Pair (np) (n n)))))
      ($ ($quote
        ($partition-toplevel-clauses 'lang
          '((terminals)               (Pair Pair? (p) (x x))
            (parser parse-lang)       (terminals (null? (x)))
            (predicate lang?)         (- Some Non Terminals)
            (+ (Num-Pair (np) (n n))) (extends another-lang)) ) )) ) )

  ;; This probably needs some explanation. 'terminals' identifier is unbound
  ;; when $partition-toplevel-clauses is defined, so it recognizes only unbound
  ;; instances of this identifier. Since this 'terminals' is not the 'terminals'
  ;; we are looking for, it is considered a nonterminal name.
  ;;
  ;; Such behavior is exemplified at the end of the item 4.3.2 in R7RS. This is
  ;; one of the reasons that generally Scheme prefers to avoid any so-called
  ;; _auxiliary syntax_, precisely because it can mess things up like here.
  (define-test ("hygiene is in effect")
    (assert-equal '(#f #f #f #f () ((terminals (num number? (n)))))
      (let ((terminals 'something-irrelevant))
        ($ ($quote
          ($partition-toplevel-clauses 'lang '((terminals (num number? (n))))) )) ) ) )
)
(verify-test-case! toplevel:peculiar)

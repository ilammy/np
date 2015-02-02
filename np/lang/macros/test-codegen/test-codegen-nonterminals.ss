(import (scheme base)
        (only (srfi 1) every first second)
        (np lang macros codegen-nonterminals)
        (np lang descriptions types)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:standalone "Code generation for standalone nonterminal forms")

  (define-syntax for
    (syntax-rules ()
      ((_ defs normalized-forms . body)
       (let ((defs ($ ($generate-standalone-nonterminal-definitions normalized-forms))))
         . body )) ) )

  (define (assert-list-of n list)
    (assert-true (list? list))
    (assert-= n (length list))
    (assert-true (every nonterminal-definition? list)) )

  (define (assert-def def name meta-variables production-descriptions)
    (assert-eq    name                    (nonterminal-name def))
    (assert-equal meta-variables          (nonterminal-meta-variables def))
    (assert-equal production-descriptions (nonterminal-production-descriptions def)) )

  (define-test ("Empty list")
    (for defs '()
      (assert-list-of 0 defs) ) )

  (define-test ("Smoke test 1")
    (for defs '((Number #f (Num) (n)))
      (assert-list-of 1 defs)
      (assert-def (first defs) 'Number '(Num) '(n)) ) )

  (define-test ("Smoke test 2")
    (for defs '((Number #(Number?) (Num) (n)))
      (assert-list-of 1 defs)
      (assert-def (first defs) 'Number '(Num) '(n)) ) )

  (define-test ("Does not change meta-variable order")
    (for defs '((Number #f (Num Number) (n)))
      (assert-list-of 1 defs)
      (assert-def (first defs) 'Number '(Num Number) '(n)) ) )

  (define-test ("Does not change production order")
    (for defs '((Number #(Number?) (Num) (n (n n))))
      (assert-list-of 1 defs)
      (assert-def (first defs) 'Number '(Num) '(n (n n))) ) )

  (define-test ("Can handle several nonterminals and does not change their order")
    (for defs '((Number #f (Num) (n)) (Pair #f (Pair) ((e e))))
      (assert-list-of 2 defs)
      (assert-def (first defs) 'Number '(Num) '(n))
      (assert-def (second defs) 'Pair '(Pair) '((e e))) ) )
)
(verify-test-case! nonterminals:standalone)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-addition "Code generation for extension addition nonterminal forms")

  (define-syntax for
    (syntax-rules ()
      ((_ defs normalized-forms . body)
       (let ((defs ($ ($generate-extension-nonterminal-additions normalized-forms))))
         . body )) ) )

  (define (assert-list-of n list)
    (assert-true (list? list))
    (assert-= n (length list))
    (assert-true (every nonterminal-definition? list)) )

  (define (assert-def def name meta-variables production-descriptions)
    (assert-eq    name                    (nonterminal-name def))
    (assert-equal meta-variables          (nonterminal-meta-variables def))
    (assert-equal production-descriptions (nonterminal-production-descriptions def)) )

  (define-test ("Empty list")
    (for defs '()
      (assert-list-of 0 defs) ) )

  (define-test ("Smoke test 1")
    (for defs '((Number #f (Num) (n)))
      (assert-list-of 1 defs)
      (assert-def (first defs) 'Number '(Num) '(n)) ) )

  (define-test ("Smoke test 2")
    (for defs '((Number Number? (Num) (n)))
      (assert-list-of 1 defs)
      (assert-def (first defs) 'Number '(Num) '(n)) ) )

  (define-test ("Does not change meta-variable order")
    (for defs '((Number #f (Num Number) (n)))
      (assert-list-of 1 defs)
      (assert-def (first defs) 'Number '(Num Number) '(n)) ) )

  (define-test ("Does not change production order")
    (for defs '((Number Number? (Num) (n (n n))))
      (assert-list-of 1 defs)
      (assert-def (first defs) 'Number '(Num) '(n (n n))) ) )

  (define-test ("Can handle several nonterminals and does not change their order")
    (for defs '((Number #f (Num) (n)) (Pair Pair? (Pair) ((e e))))
      (assert-list-of 2 defs)
      (assert-def (first defs) 'Number '(Num) '(n))
      (assert-def (second defs) 'Pair '(Pair) '((e e))) ) )
)
(verify-test-case! nonterminals:extension-addition)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-removal "Code generation for extension removal nonterminal forms")

  (define-syntax for
    (syntax-rules ()
      ((_ rems normalized-forms . body)
       (let ((rems ($ ($generate-extension-nonterminal-removals normalized-forms))))
         . body )) ) )

  (define (assert-list-of n list)
    (assert-true (list? list))
    (assert-= n (length list))
    (assert-true (every symbol? list)) )

  (define (assert-rem removed name)
    (assert-eq name removed) )

  (define-test ("Empty list")
    (for rems '()
      (assert-list-of 0 rems) ) )

  (define-test ("Smoke test")
    (for rems '(Number)
      (assert-list-of 1 rems)
      (assert-rem (first rems) 'Number) ) )

  (define-test ("Does not change removed nonterminal order")
    (for rems '(Number Pair)
      (assert-list-of 2 rems)
      (assert-rem (first rems) 'Number)
      (assert-rem (second rems) 'Pair) ) )
)
(verify-test-case! nonterminals:extension-removal)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:extension-modification "Code generation for extension modification nonterminal forms")

  (define-syntax for
    (syntax-rules ()
      ((_ mods normalized-forms . body)
       (let ((mods ($ ($generate-extension-nonterminal-modifications normalized-forms))))
         . body )) ) )

  (define (assert-list-of n list)
    (assert-true (list? list))
    (assert-= n (length list))
    (assert-true (every nonterminal-modification? list)) )

  (define (assert-mod modified name added-meta-vars added-production-descriptions removed-meta-vars removed-production-descriptions)
    (assert-eq    name                            (modified-nonterminal-name modified))
    (assert-equal added-meta-vars                 (modified-nonterminal-added-meta-variables modified))
    (assert-equal added-production-descriptions   (modified-nonterminal-added-production-descriptions modified))
    (assert-equal removed-meta-vars               (modified-nonterminal-removed-meta-variables modified))
    (assert-equal removed-production-descriptions (modified-nonterminal-removed-production-descriptions modified)) )

  (define-test ("Empty list")
    (for mods '()
      (assert-list-of 0 mods) ) )

  (define-test ("Smoke test")
    (for mods '((Number () () () ()))
      (assert-list-of 1 mods)
      (assert-mod (first mods) 'Number '() '() '() '()) ) )

  (define-test ("Does not change meta-variable order")
    (for mods '((Number (a b) (c d e) () ()))
      (assert-list-of 1 mods)
      (assert-mod (first mods) 'Number '(a b) '() '(c d e) '()) ) )

  (define-test ("Does not change production order")
    (for mods '((Number () () (a b) (c d e)))
      (assert-list-of 1 mods)
      (assert-mod (first mods) 'Number '() '(a b) '() '(c d e)) ) )

  (define-test ("Does not change nonterminal order")
    (for mods '((Number (a b c) (d) (e f) (g h i)) (Pair (j) () () (k)))
      (assert-list-of 2 mods)
      (assert-mod (first mods) 'Number '(a b c) '(e f) '(d) '(g h i))
      (assert-mod (second mods) 'Pair '(j) '() '() '(k)) ) )
)
(verify-test-case! nonterminals:extension-modification)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:predicate-definitions:standalone "Definition generators for standalone nonterminal predicates")

  (define lang #f) ; expected to fail when define-language is implemented

  (define-syntax for
    (syntax-rules ()
      ((_ normalized-forms . body)
       (begin
         ($ ($generate-standalone-nonterminal-predicate-definitions 'lang normalized-forms))
         . body )) ) )

  (define (assert-defined procedure)
    (assert-true (procedure? procedure)) )

  ; This should be syntactically valid
  (define-test ("Empty list")
    (for '()
      #t ) )

  ; This should be syntactically valid
  (define-test ("Effectively empty list")
    (for '((Number #f (Num) (n)) (Pair #f (Cons) ((a d))))
      #t ) )

  (define-test ("Example definitions")
    (for '((Number Number? (Num) (n)) (Pair Pair? (Cons) ((a d))))
      (assert-defined Number?)
      (assert-defined Pair?) ) )

  ; These are allowed, but they are the user's problem
  (define-test ("Duplicate definitions are allowed")
    (for '((Number Number? (Num) (n)) (Pair Number? (Cons) ((a d))))
      (assert-defined Number?) ) )
)
(verify-test-case! nonterminals:predicate-definitions:standalone)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (nonterminals:predicate-definitions:extension "Definition generators for extension nonterminal predicates")

  (define lang #f) ; expected to fail when define-language is implemented

  (define-syntax for
    (syntax-rules ()
      ((_ normalized-forms . body)
       (begin
         ($ ($generate-extension-nonterminal-predicate-definitions 'lang normalized-forms))
         . body )) ) )

  (define (assert-defined procedure)
    (assert-true (procedure? procedure)) )

  ; This should be syntactically valid
  (define-test ("Empty list")
    (for '()
      #t ) )

  ; This should be syntactically valid
  (define-test ("Effectively empty list")
    (for '((Number #f (Num) (n)) (Pair #f (Cons) ((a d))))
      #t ) )

  (define-test ("Example definitions")
    (for '((Number Number? (Num) (n)) (Pair Pair? (Cons) ((a d))))
      (assert-defined Number?)
      (assert-defined Pair?) ) )

  ; These are allowed, but they are the user's problem
  (define-test ("Duplicate definitions are allowed")
    (for '((Number Number? (Num) (n)) (Pair Number? (Cons) ((a d))))
      (assert-defined Number?) ) )
)
(verify-test-case! nonterminals:predicate-definitions:extension)

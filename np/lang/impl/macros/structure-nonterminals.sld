(define-library (np lang impl macros structure-nonterminals)
  ;;;
  ;;; Structural analysis of nonterminal definitions (standalone and extension)
  ;;;
  (export  $can-be:standalone-nonterminal?
             $is-a:standalone-nonterminal?
          $must-be:standalone-nonterminal

           $can-be:nonterminal-implicit-addition?
             $is-a:nonterminal-implicit-addition?
          $must-be:nonterminal-implicit-addition

           $can-be:nonterminal-explicit-addition?
             $is-a:nonterminal-explicit-addition?
          $must-be:nonterminal-explicit-addition

           $can-be:nonterminal-removal?
             $is-a:nonterminal-removal?
          $must-be:nonterminal-removal

           $can-be:nonterminal-modification?
             $is-a:nonterminal-modification?
          $must-be:nonterminal-modification

          $get-nonterminal-modification-meta-vars
          $set-nonterminal-modification-meta-vars

          $get-nonterminal-modification-productions
          $set-nonterminal-modification-productions)

  (import (scheme base)
          (sr ck)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates)
          (np lang impl macros structure-meta-vars)
          (np lang impl macros structure-productions)
          (np lang impl macros utils))

  (begin

    ;;;
    ;;; Nonterminal definitions (standalone)
    ;;;

    ;; Predicate has ellipsis to catch the empty/multiple name error
    (define-syntax $can-be:standalone-nonterminal?
      (syntax-rules (quote)
        ((_ s '(name #(predicate-name ...) meta-var-list . production-list)) ($ s '#t))
        ((_ s '(name                       meta-var-list . production-list)) ($ s '#t))
        ((_ s  _)                                                            ($ s '#f)) ) )

    (define-standard-checkers %verify:standalone-nonterminal
      ($is-a:standalone-nonterminal? $must-be:standalone-nonterminal) )

    (define-verifier %verify:standalone-nonterminal
      (syntax-rules (quote)
        ((_ s '(k t) 'term '(name #(predicate-name ...) meta-var-list . production-list))
         ($ s ($and '(%verify:nonterminal-name '(k ((name #(predicate-name ...) meta-var-list) . t)) 'name)
                    '(%verify:nonterminal-predicate-name-count '(k ((name #(predicate-name ...) meta-var-list) . t)) '#(predicate-name ...))
                    '(%verify:nonterminal-predicate-name '(k ((name #(predicate-name ...) meta-var-list) . t)) 'predicate-name ...)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) 'meta-var-list)
                    '($every? '(%verify:meta-var-name '(k (name . t))) 'meta-var-list)
                    '(%verify:nonterminal-production-list '(k (name . t)) 'production-list)
                    '($every? '(%verify:standalone-production '(k (name . t))) 'production-list) )))

        ((_ s '(k t) 'term '(name #(predicate-name ...) . _))     ; Taking a shortcut here...
         ($ k '("Invalid syntax of the nonterminal" (term . t))))

        ((_ s '(k t) 'term '(name meta-var-list . production-list))
         ($ s ($and '(%verify:nonterminal-name '(k ((name meta-var-list) . t)) 'name)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) 'meta-var-list)
                    '($every? '(%verify:meta-var-name '(k (name . t))) 'meta-var-list)
                    '(%verify:nonterminal-production-list '(k (name . t)) 'production-list)
                    '($every? '(%verify:standalone-production '(k (name . t))) 'production-list) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the nonterminal" (term . t)))) ) )

    (define-verifier %verify:nonterminal-predicate-name-count
      (syntax-rules (quote)
        ((_ s '(k t) 'term '#())       ($ k '("Name of the nonterminal predicate cannot be empty" (term . t))))
        ((_ s '(k t) 'term '#(x))      ($ s '#t))
        ((_ s '(k t) 'term '#(xs ...)) ($ k '("Only one nonterminal predicate name can be specified" (term . t)))) ) )

    ;;;
    ;;;  Nonterminal descriptions (implicit addition)
    ;;;

    (define-syntax $can-be:nonterminal-implicit-addition?
      (syntax-rules (quote)
        ((_ s 'term) ($ s ($can-be:standalone-nonterminal-description? 'term))) ) )

    (define-syntax $is-a:nonterminal-implicit-addition?
      (syntax-rules (quote)
        ((_ s 'term) ($ s ($is-a:standalone-nonterminal-description? 'term))) ) )

    (define-syntax $must-be:nonterminal-implicit-addition
      (syntax-rules (quote)
        ((_ s 'lang 'term) ($ s ($must-be:standalone-nonterminal-description 'lang 'term))) ) )

    ;;;
    ;;;  Nonterminal descriptions (explicit addition) - interface
    ;;;

    (define-syntax $can-be:nonterminal-explicit-addition?
      (syntax-rules (quote +)
        ((_ s '(+ . rest)) ($ s '#t))
        ((_ s  _)          ($ s '#f)) ) )

    (define-syntax $is-a:nonterminal-explicit-addition?
      (syntax-rules (quote)
        ((_ s 'term) ($ s ($verify-result:as-boolean
                            ($verify:nonterminal-explicit-addition 'term) ))) ) )

    (define-syntax $must-be:nonterminal-explicit-addition
      (syntax-rules (quote)
        ((_ s 'lang 'term) ($ s ($verify-result:syntax-error
                                  ($verify:nonterminal-explicit-addition 'lang 'term) ))) ) )

    ;;;
    ;;;  Nonterminal descriptions (explicit addition) - interface
    ;;;

    (define-syntax $verify:nonterminal-explicit-addition
      (syntax-rules (quote)
        ((_ s 'term)       ($ s (%verify:nonterminal-explicit-addition '(s ())     'term)))
        ((_ s 'lang 'term) ($ s (%verify:nonterminal-explicit-addition '(s (lang)) 'term))) ) )

    (define-syntax %verify:nonterminal-explicit-addition
      (syntax-rules (quote)
        ((_ s '(k t) '(? . descriptions))
         ($ s ($and '(%verify:nonterminal-addition-list '(k t) '(? . descriptions) '(? . descriptions))
                    '(%verify:nonterminal-description-list '(k ((? . descriptions) . t)) 'descriptions)
                    '($every? '(%verify:standalone-nonterminal-description '(k t)) 'descriptions) )))

        ((_ s '(k t) 'invalid-syntax)
         ($ k '("Invalid nonterminal description syntax" (invalid-syntax . t)))) ) )

    ;; The 2nd and 3rd arguments are actually the same. This trick is necessary
    ;; to get the (+) form from the original source, because writing just `(+)`
    ;; in the expansion yields a list that contains another plus--that free one
    ;; from (scheme base) as '+' is not a pattern variable here.
    (define-syntax %verify:nonterminal-addition-list
      (syntax-rules (quote +)
        ((_ s '(k t) '(+)         'x) ($ k '("At least one nonterminal should be specified for addition" (x . t))))
        ((_ s '(k t) '(+ . other) 'x) ($ s '#t))
        ((_ s '(k t)  _           'x) ($ k '("Invalid terminal description syntax" (x . t)))) ) )

    ;;;
    ;;; Nonterminal descriptions (removal) - interface
    ;;;

    (define-syntax $can-be:nonterminal-removal?
      (syntax-rules (quote -)
        ((_ s '(- . rest)) ($ s '#t))
        ((_ s  _)          ($ s '#f)) ) )

    (define-syntax $is-a:nonterminal-removal?
      (syntax-rules (quote)
        ((_ s 'term) ($ s ($verify-result:as-boolean
                            ($verify:nonterminal-removal 'term) ))) ) )

    (define-syntax $must-be:nonterminal-removal
      (syntax-rules (quote)
        ((_ s 'lang 'term) ($ s ($verify-result:syntax-error
                                  ($verify:nonterminal-removal 'lang 'term) ))) ) )

    ;;;
    ;;; Nonterminal descriptions (removal) - implementation
    ;;;

    (define-syntax $verify:nonterminal-removal
      (syntax-rules (quote)
        ((_ s 'term)       ($ s (%verify:nonterminal-removal '(s ())     'term)))
        ((_ s 'lang 'term) ($ s (%verify:nonterminal-removal '(s (lang)) 'term))) ) )

    (define-syntax %verify:nonterminal-removal
      (syntax-rules (quote)
        ((_ s '(k t) '(? . descriptions))
         ($ s ($and '(%verify:nonterminal-removal-list '(k t) '(? . descriptions) '(? . descriptions))
                    '(%verify:nonterminal-description-list '(k ((? . descriptions) . t)) 'descriptions)
                    '($every? '(%verify:nonterminal-name/nonterminal-concise-description '(k t)) 'descriptions) )))

        ((_ s '(k t) 'invalid-syntax)
         ($ k '("Invalid terminal description syntax" (invalid-syntax . t)))) ) )

    (define-syntax %verify:nonterminal-name/nonterminal-concise-description
      (syntax-rules (quote)
        ((_ s '(k t) '())       ($ s (%verify:concise-nonterminal-description '(k t) '())))
        ((_ s '(k t) '(a . d))  ($ s (%verify:concise-nonterminal-description '(k t) '(a . d))))
        ((_ s '(k t) '#(x ...)) ($ s (%verify:concise-nonterminal-description '(k t) '#(x ...))))
        ((_ s '(k t) 'atom)     ($ s '#t)) ) )

    ;; (See comment for %verify:nonterminal-addition-list)
    (define-syntax %verify:nonterminal-removal-list
      (syntax-rules (quote -)
        ((_ s '(k t) '(-)         'x) ($ k '("At least one nonterminal should be specified for removal" (x . t))))
        ((_ s '(k t) '(- . other) 'x) ($ s '#t))
        ((_ s '(k t)  _           'x) ($ k '("Invalid nonterminal description syntax" (x . t)))) ) )

    ;; Effectively this is %verify:standalone-nonterminal-description that allows empty production list
    (define-syntax %verify:concise-nonterminal-description
      (syntax-rules (quote)
        ((_ s '(k t) '(name predicate () pro . ductions))
         ($ s ($and '(%verify:nonterminal-name '(k ((name predicate ()) . t)) 'name)
                    '(%verify:nonterminal-predicate-name '(k ((name predicate ()) . t)) 'predicate)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) '())
                    '(%verify:concise-nonterminal-production-list '(k (name . t)) '(pro . ductions))
                    '($every? '(%verify:standalone-production '(k (name . t))) '(pro . ductions)) )))

        ((_ s '(k t) '(name predicate (meta . vars) pro . ductions))
         ($ s ($and '(%verify:nonterminal-name '(k ((name predicate (meta . vars)) . t)) 'name)
                    '(%verify:nonterminal-predicate-name '(k ((name predicate (meta . vars)) . t)) 'predicate)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) '(meta . vars))
                    '($every? '(%verify:meta-var-name '(k (name . t))) '(meta . vars))
                    '(%verify:concise-nonterminal-production-list '(k (name . t)) '(pro . ductions))
                    '($every? '(%verify:standalone-production '(k (name . t))) '(pro . ductions)) )))

        ((_ s '(k t) '(name predicate ()))
         ($ s ($and '(%verify:nonterminal-name '(k ((name predicate ()) . t)) 'name)
                    '(%verify:nonterminal-predicate-name '(k ((name predicate ()) . t)) 'predicate)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) '()) )))

        ((_ s '(k t) '(name predicate (meta . vars)))
         ($ s ($and '(%verify:nonterminal-name '(k ((name predicate (ext-meta . vars)) . t)) 'name)
                    '(%verify:nonterminal-predicate-name '(k ((name predicate (meta . vars)) . t)) 'predicate)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) '(meta . vars))
                    '($every? '(%verify:meta-var-name '(k (name . t))) '(meta . vars)) )))

        ((_ s '(k t) '(name () pro . ductions))
         ($ s ($and '(%verify:nonterminal-name '(k ((name ()) . t)) 'name)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) '())
                    '(%verify:concise-nonterminal-production-list '(k (name . t)) '(pro . ductions))
                    '($every? '(%verify:standalone-production '(k (name . t))) '(pro . ductions)) )))

        ((_ s '(k t) '(name (meta . vars) pro . ductions))
         ($ s ($and '(%verify:nonterminal-name '(k ((name (meta . vars)) . t)) 'name)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) '(meta . vars))
                    '($every? '(%verify:meta-var-name '(k (name . t))) '(meta . vars))
                    '(%verify:concise-nonterminal-production-list '(k (name . t)) '(pro . ductions))
                    '($every? '(%verify:standalone-production '(k (name . t))) '(pro . ductions)) )))

        ((_ s '(k t) '(name ()))
         ($ s ($and '(%verify:nonterminal-name '(k ((name ()) . t)) 'name)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) '()) )))

        ((_ s '(k t) '(name (meta . vars)))
         ($ s ($and '(%verify:nonterminal-name '(k ((name (meta . vars)) . t)) 'name)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) '(meta . vars))
                    '($every? '(%verify:meta-var-name '(k (name . t))) '(meta . vars)) )))

        ((_ s '(k t) 'invalid-syntax)
         ($ k '("Invalid nonterminal description syntax" (invalid-syntax . t)))) ) )

    ;;;
    ;;; Nonterminal descriptions (modification) - interface
    ;;;

    (define-syntax $can-be:nonterminal-modification?
      (syntax-rules (quote)
        ((_ s '(name predicate (vars ...) productions ...))
         ($ s ($or '($any? '$can-be:extension-meta-var? '(vars ...))
                   '($any? '$can-be:extension-production? '(productions ...)) )))

        ((_ s '(name (vars ...) productions ...))
         ($ s ($or '($any? '$can-be:extension-meta-var? '(vars ...))
                   '($any? '$can-be:extension-production? '(productions ...)) )))

        ((_ s _) ($ s '#f)) ) )

    (define-syntax $is-a:nonterminal-modification?
      (syntax-rules (quote)
        ((_ s 'term) ($ s ($verify-result:as-boolean
                            ($verify:nonterminal-modification 'term) ))) ) )

    (define-syntax $must-be:nonterminal-modification
      (syntax-rules (quote)
        ((_ s 'lang 'term) ($ s ($verify-result:syntax-error
                                  ($verify:nonterminal-modification 'lang 'term) ))) ) )

    ;;;
    ;;; Nonterminal descriptions (modification) - implementation
    ;;;

    (define-syntax $verify:nonterminal-modification
      (syntax-rules (quote)
        ((_ s 'term)       ($ s (%verify:nonterminal-modification '(s ())     'term)))
        ((_ s 'lang 'term) ($ s (%verify:nonterminal-modification '(s (lang)) 'term))) ) )

    (define-syntax %verify:nonterminal-modification
      (syntax-rules (quote)
        ((_ s '(k t) '(name predicate () ext-pro . ductions))
         ($ s ($and '(%verify:nonterminal-name '(k ((name predicate ()) . t)) 'name)
                    '(%verify:nonterminal-predicate-name '(k ((name predicate ()) . t)) 'predicate)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) '())
                    '(%verify:concise-nonterminal-production-list '(k (name . t)) '(ext-pro . ductions))
                    '($every? '(%verify:ext-productions '(k (name . t))) '(ext-pro . ductions)) )))

        ((_ s '(k t) '(name predicate (ext-meta . vars) ext-pro . ductions))
         ($ s ($and '(%verify:nonterminal-name '(k ((name predicate (ext-meta . vars)) . t)) 'name)
                    '(%verify:nonterminal-predicate-name '(k ((name predicate (ext-meta . vars)) . t)) 'predicate)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) '(ext-meta . vars))
                    '($every? '(%verify:ext-meta-vars '(k (name . t))) '(ext-meta . vars))
                    '(%verify:concise-nonterminal-production-list '(k (name . t)) '(ext-pro . ductions))
                    '($every? '(%verify:ext-productions '(k (name . t))) '(ext-pro . ductions)) )))

        ((_ s '(k t) '(name predicate ()))
         ($ s ($and '(%verify:nonterminal-name '(k ((name predicate ()) . t)) 'name)
                    '(%verify:nonterminal-predicate-name '(k ((name predicate ()) . t)) 'predicate)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) '()) )))

        ((_ s '(k t) '(name predicate (ext-meta . vars)))
         ($ s ($and '(%verify:nonterminal-name '(k ((name predicate (ext-meta . vars)) . t)) 'name)
                    '(%verify:nonterminal-predicate-name '(k ((name predicate (ext-meta . vars)) . t)) 'predicate)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) '(ext-meta . vars))
                    '($every? '(%verify:ext-meta-vars '(k (name . t))) '(ext-meta . vars)) )))

        ((_ s '(k t) '(name () ext-pro . ductions))
         ($ s ($and '(%verify:nonterminal-name '(k ((name ()) . t)) 'name)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) '())
                    '(%verify:concise-nonterminal-production-list '(k (name . t)) '(ext-pro . ductions))
                    '($every? '(%verify:ext-productions '(k (name . t))) '(ext-pro . ductions)) )))

        ((_ s '(k t) '(name (ext-meta . vars) ext-pro . ductions))
         ($ s ($and '(%verify:nonterminal-name '(k ((name (ext-meta . vars)) . t)) 'name)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) '(ext-meta . vars))
                    '($every? '(%verify:ext-meta-vars '(k (name . t))) '(ext-meta . vars))
                    '(%verify:concise-nonterminal-production-list '(k (name . t)) '(ext-pro . ductions))
                    '($every? '(%verify:ext-productions '(k (name . t))) '(ext-pro . ductions)) )))

        ((_ s '(k t) '(name ()))
         ($ s ($and '(%verify:nonterminal-name '(k ((name ()) . t)) 'name)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) '()) )))

        ((_ s '(k t) '(name (ext-meta . vars)))
         ($ s ($and '(%verify:nonterminal-name '(k ((name (ext-meta . vars)) . t)) 'name)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) '(ext-meta . vars))
                    '($every? '(%verify:ext-meta-vars '(k (name . t))) '(ext-meta . vars)) )))

        ((_ s '(k t) 'invalid-syntax)
         ($ k '("Invalid nonterminal description syntax" (invalid-syntax . t)))) ) )

    ;; (See comment for %verify:nonterminal-addition-list)
    (define-syntax %verify:ext-meta-vars
      (syntax-rules (quote + -)
        ((_ s '(k t) 'x) ($ s (%verify:ext-meta-vars '(k t) 'x 'x)))

        ((_ s '(k t) '(+) 'x)
         ($ k '("At least one meta-variable should be specified for addition" (x . t))))

        ((_ s '(k t) '(-) 'x)
         ($ k '("At least one meta-variable should be specified for removal" (x . t))))

        ((_ s '(k t) '(+ . list) 'x)
         ($ s ($and '(%verify:nonterminal-meta-var-list '(k t) 'x)
                    '($every? '(%verify:meta-var-name '(k (x . t))) 'list) )))

        ((_ s '(k t) '(- . list) 'x)
         ($ s ($and '(%verify:nonterminal-meta-var-list '(k t) 'x)
                    '($every? '(%verify:meta-var-name '(k (x . t))) 'list) )))

        ((_ s '(k t) _ 'x) ($ k '("Invalid extension meta-variable syntax" (x . t)))) ) )

    ;; (See comment for %verify:nonterminal-addition-list)
    (define-syntax %verify:ext-productions
      (syntax-rules (quote + -)
        ((_ s '(k t) 'x) ($ s (%verify:ext-productions '(k t) 'x 'x)))

        ((_ s '(k t) '(+) 'x)
         ($ k '("At least one production should be specified for addition" (x . t))))

        ((_ s '(k t) '(-) 'x)
         ($ k '("At least one production should be specified for removal" (x . t))))

        ((_ s '(k t) '(+ . list) 'x)
         ($ s ($and '(%verify:nonterminal-production-list '(k t) 'x)
                    '($every? '(%verify:standalone-production '(k (x . t))) 'list) )))

        ((_ s '(k t) '(- . list) 'x)
         ($ s ($and '(%verify:nonterminal-production-list '(k t) 'x)
                    '($every? '(%verify:standalone-production '(k (x . t))) 'list) )))

        ((_ s '(k t) _ 'x) ($ k '("Invalid extension production syntax" (x . t)))) ) )

    ;;;
    ;;; Getter/setter for subclauses in nonterminal modification descriptions
    ;;;

    (define-syntax $get-nonterminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s _ '(name predicate (vars ...) prods ...))
         ($ s '(vars ...)))
        ((_ s _ '(name (vars ...) prods ...))
         ($ s '(vars ...)))
        ((_ s 'lang 'expr)
         (syntax-error "Invalid nonterminal modification description" lang expr)) ) )

    (define-syntax $set-nonterminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s _ '(name predicate (vars ...) prods ...) '(new-vars ...))
         ($ s '(name predicate (new-vars ...) prods ...)))
        ((_ s _ '(name (vars ...) prods ...) '(new-vars ...))
         ($ s '(name (new-vars ...) prods ...)))
        ((_ s 'lang 'expr)
         (syntax-error "Invalid nonterminal modification description" lang expr)) ) )

    (define-syntax $get-nonterminal-modification-productions
      (syntax-rules (quote)
        ((_ s _ '(name predicate (vars ...) prods ...))
         ($ s '(prods ...)))
        ((_ s _ '(name (vars ...) prods ...))
         ($ s '(prods ...)))
        ((_ s 'lang 'expr)
         (syntax-error "Invalid nonterminal modification description" lang expr)) ) )

    (define-syntax $set-nonterminal-modification-productions
      (syntax-rules (quote)
        ((_ s _ '(name predicate (vars ...) prods ...) '(new-prods ...))
         ($ s '(name predicate (vars ...) (new-prods ...))))
        ((_ s _ '(name predicate (vars ...) prods ...) '(new-prods ...))
         ($ s '(name predicate (vars ...) (new-prods ...))))
        ((_ s 'lang 'expr)
         (syntax-error "Invalid nonterminal modification description" lang expr)) ) )

    ;;;
    ;;; Common verifiers
    ;;;

    (define-verifier/atom %verify:nonterminal-name
      ("Name of the nonterminal must be a symbol") )

    (define-verifier/atom %verify:nonterminal-predicate-name
      ("Name of the nonterminal predicate must be a symbol") )

    (define-syntax %verify:nonterminal-description-list
      (syntax-rules (quote)
        ((_ s '(k t) '(x ...))     ($ s '#t))
        ((_ s '(k t) '(x ... . a)) ($ k '("Unexpected dotted list in nonterminal description" (a . t))))
        ((_ s '(k t) 'unexpected)  ($ k '("Expected nonterminal description list" (unexpected . t)))) ) )

    (define-verifier/proper-list %verify:nonterminal-meta-var-list
      ("Unexpected dotted list in nonterminal definition"
       "Expected a list of meta-variables") )

    (define-verifier/proper-nonempty-list %verify:nonterminal-production-list
      ("At least one production should be specified for a nonterminal"
       "Unexpected dotted list in nonterminal definition"
       "Expected a list of productions") )

    (define-syntax %verify:concise-nonterminal-production-list
      (syntax-rules (quote)
        ((_ s '(k t) '(x ...))     ($ s '#t))
        ((_ s '(k t) '(x ... . a)) ($ k '("Unexpected dotted list in nonterminal description" (a (x ... . a) . t))))
        ((_ s '(k t) 'unexpected)  ($ k '("Unexpected dotted list in nonterminal description" (unexpected . t)))) ) )

) )

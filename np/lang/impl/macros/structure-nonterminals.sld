(define-library (np lang impl macros structure-nonterminals)
  ;;;
  ;;; Structural analysis of nonterminal definitions (standalone and extension)
  ;;;
  (export  $can-be:standalone-nonterminal?
             $is-a:standalone-nonterminal?
          $must-be:standalone-nonterminal

           $can-be:nonterminal-addition?
             $is-a:nonterminal-addition?
          $must-be:nonterminal-addition

           $can-be:nonterminal-removal?
             $is-a:nonterminal-removal?
          $must-be:nonterminal-removal

           $can-be:nonterminal-modification?
             $is-a:nonterminal-modification?
          $must-be:nonterminal-modification

          $squash-extension-clauses

          $get-nonterminal-modification-meta-vars
          $set-nonterminal-modification-meta-vars

          $get-nonterminal-modification-productions
          $set-nonterminal-modification-productions

          $expected-a:nonterminal-definition)

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
    ;;;  Nonterminal definitions (addition)
    ;;;

    (define-syntax $can-be:nonterminal-addition?
      (syntax-rules (quote +)
        ((_ s '(+ . added-nonterminal-list)) ($ s '#t))
        ((_ s  _)                            ($ s '#f)) ) )

    (define-standard-checked-verifier ($is-a:nonterminal-addition? $must-be:nonterminal-addition)
      (syntax-rules (quote +)
        ((_ s '(k t) 'term '(+ . added-nonterminal-list))
         ($ s ($and '(%verify:nonterminal-addition-list '(k (term . t)) 'added-nonterminal-list)
                    '($every? '(%verify:standalone-nonterminal '(k t)) 'added-nonterminal-list) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the nonterminal extension" (term . t)))) ) )

    ;;;
    ;;; Nonterminal definitions (removal)
    ;;;

    (define-syntax $can-be:nonterminal-removal?
      (syntax-rules (quote -)
        ((_ s '(- . removed-nonterminal-list)) ($ s '#t))
        ((_ s  _)                              ($ s '#f)) ) )

    (define-standard-checked-verifier ($is-a:nonterminal-removal? $must-be:nonterminal-removal)
      (syntax-rules (quote -)
        ((_ s '(k t) 'term '(- . removed-nonterminal-list))
         ($ s ($and '(%verify:nonterminal-removal-list '(k (term . t)) 'removed-nonterminal-list)
                    '($every? '(%verify:nonterminal-name/concise-definition '(k t)) 'removed-nonterminal-list) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the nonterminal extension" (term . t)))) ) )

    (define-verifier %verify:nonterminal-name/concise-definition
      (syntax-rules (quote)
        ((_ s '(k t) 'term '())       ($ s (%verify:nonterminal-concise-definition '(k t) 'term)))
        ((_ s '(k t) 'term '(a . d))  ($ s (%verify:nonterminal-concise-definition '(k t) 'term)))
        ((_ s '(k t) 'term '#(x ...)) ($ s (%verify:nonterminal-concise-definition '(k t) 'term)))
        ((_ s '(k t) 'term  _)        ($ s '#t)) ) )

    ;; Effectively this is %verify:standalone-nonterminal-definition that allows empty production list.
    ;; The difference is in usage of %verify:nonterminal-*concise*-production-list.
    (define-verifier %verify:nonterminal-concise-definition
      (syntax-rules (quote)
        ((_ s '(k t) 'term '(name #(predicate-name ...) meta-var-list . production-list))
         ($ s ($and '(%verify:nonterminal-name '(k ((name #(predicate-name ...) meta-var-list) . t)) 'name)
                    '(%verify:nonterminal-predicate-name-count '(k ((name #(predicate-name ...) meta-var-list) . t)) '#(predicate-name ...))
                    '(%verify:nonterminal-predicate-name '(k ((name #(predicate-name ...) meta-var-list) . t)) 'predicate-name ...)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) 'meta-var-list)
                    '($every? '(%verify:meta-var-name '(k (name . t))) 'meta-var-list)
                    '(%verify:nonterminal-concise-production-list '(k (name . t)) 'production-list)
                    '($every? '(%verify:standalone-production '(k (name . t))) 'production-list) )))

        ((_ s '(k t) 'term '(name #(predicate-name ...) . _))     ; Taking a shortcut here...
         ($ k '("Invalid syntax of the nonterminal" (term . t))))

        ((_ s '(k t) 'term '(name meta-var-list . production-list))
         ($ s ($and '(%verify:nonterminal-name '(k ((name meta-var-list) . t)) 'name)
                    '(%verify:nonterminal-meta-var-list '(k (name . t)) 'meta-var-list)
                    '($every? '(%verify:meta-var-name '(k (name . t))) 'meta-var-list)
                    '(%verify:nonterminal-concise-production-list '(k (name . t)) 'production-list)
                    '($every? '(%verify:standalone-production '(k (name . t))) 'production-list) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the nonterminal" (term . t)))) ) )

    ;;;
    ;;; Nonterminal definitions (modification)
    ;;;

    (define-syntax $can-be:nonterminal-modification?
      (syntax-rules (quote !)
        ((_ s '(! . modified-nonterminal-list)) ($ s '#t))
        ((_ s  _)                               ($ s '#f)) ) )

    (define-standard-checked-verifier ($is-a:nonterminal-modification? $must-be:nonterminal-modification)
      (syntax-rules (quote !)
        ((_ s '(k t) 'term '(! . modified-nonterminal-list))
         ($ s ($and '(%verify:nonterminal-modification-list '(k (term . t)) 'modified-nonterminal-list)
                    '($every? '(%verify:nonterminal-modification '(k t)) 'modified-nonterminal-list) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the nonterminal extension" (term . t)))) ) )

    (define-verifier %verify:nonterminal-modification
      (syntax-rules (quote)
        ((_ s '(k t) 'term '(name meta-var-modification-list . production-modification-list))
         ($ s ($and '(%verify:nonterminal-name '(k ((name meta-var-modification-list) . t)) 'name)
                    '(%verify:nonterminal-meta-var-modification-list '(k (name . t)) 'meta-var-modification-list)
                    '($every? '(%verify:meta-var-modification '(k (name . t))) 'meta-var-modification-list)
                    '(%verify:nonterminal-production-modification-list '(k (name . t)) 'production-modification-list)
                    '($every? '(%verify:production-modification '(k (name . t))) 'production-modification-list)
                    '(%verify:meta-vars-or-productions-are-modified '(k (name . t))
                       'meta-var-modification-list 'production-modification-list ) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the nonterminal modification" (term . t)))) ) )

    (define-syntax %verify:meta-vars-or-productions-are-modified
      (syntax-rules (quote)
        ((_ s '(k t) '() '())
         ($ k '("Nonterminal modification should modify either meta-variables or productions" t)))

        ((_ s '(k t) _ _) ($ s '#t)) ) )

    ;;;
    ;;; Getters, setters, squashers, etc. (Valid input is assumed everywhere.)
    ;;;

    (define-syntax $squash-extension-clauses
      (syntax-rules (quote)
        ((_ s 'clauses) ($ s ($concatenate ($map '$cdr 'clauses)))) ) )

    (define-syntax $get-nonterminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s 'lang '(name meta-var-modification-list . production-modification-list))
         ($ s 'meta-var-modification-list)) ))

    (define-syntax $set-nonterminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s 'lang '(name meta-var-modification-list . production-modification-list) 'meta-var-modification-list*)
         ($ s '(name meta-var-modification-list* . production-modification-list))) ) )

    (define-syntax $get-nonterminal-modification-productions
      (syntax-rules (quote)
        ((_ s 'lang '(name meta-var-modification-list . production-modification-list))
         ($ s 'production-modification-list)) ))

    (define-syntax $set-nonterminal-modification-productions
      (syntax-rules (quote)
        ((_ s 'lang '(name meta-var-modification-list . production-modification-list) 'production-modification-list*)
         ($ s '(name meta-var-modification-list production-modification-list*))) ) )

    (define-syntax $expected-a:nonterminal-definition
      (syntax-rules (quote)
        ((_ s 'lang 'invalid-definition)
         (syntax-error "Invalid syntax of the nonterminal extension" lang invalid-definition)) ) )

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

    (define-verifier/proper-list %verify:nonterminal-concise-production-list
      ("Unexpected dotted list in nonterminal definition"
       "Expected a list of productions") )

    (define-verifier/proper-nonempty-list:report-dot-only %verify:nonterminal-addition-list
      ("At least one nonterminal should be specified for addition"
       "Unexpected dotted list in nonterminal extension"
       "Expected a list of nonterminal definitions") )

    (define-verifier/proper-nonempty-list:report-dot-only %verify:nonterminal-removal-list
      ("At least one nonterminal should be specified for removal"
       "Unexpected dotted list in nonterminal extension"
       "Expected a list of nonterminal definitions or names") )

    (define-verifier/proper-nonempty-list:report-dot-only %verify:nonterminal-modification-list
      ("At least one nonterminal should be specified for modification"
       "Unexpected dotted list in nonterminal extension"
       "Expected a list of nonterminal modifications") )

    (define-verifier/proper-list %verify:nonterminal-meta-var-modification-list
      ("Unexpected dotted list in nonterminal modification"
       "Expected a list of meta-variable modifications") )

    (define-verifier/proper-list %verify:nonterminal-production-modification-list
      ("Unexpected dotted list in nonterminal modification"
       "Expected a list of production modifications") )
) )

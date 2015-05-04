(define-library (np lang macros structure-nonterminals)
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

          $expected-a:nonterminal-definition

          $squash-extension-nonterminal-clauses

          $get-nonterminal-modification-meta-vars
          $set-nonterminal-modification-meta-vars

          $get-nonterminal-modification-productions
          $set-nonterminal-modification-productions)

  (import (scheme base)
          (sr ck)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates)
          (np lang macros structure-meta-vars)
          (np lang macros structure-productions)
          (np lang macros verify-utils))

  (begin

    ;;;
    ;;; Nonterminal definitions (standalone)
    ;;;

    (define-syntax $can-be:standalone-nonterminal?
      (syntax-rules (quote)
        ((_ s '(name predicate-name (meta-vars ...) productions ...)) ($ s '#t))
        ((_ s '(name                (meta-vars ...) productions ...)) ($ s '#t))
        ((_ s  _)                                                     ($ s '#f)) ) )

    (define-standard-checkers %verify:standalone-nonterminal
      ($is-a:standalone-nonterminal? $must-be:standalone-nonterminal) )

    (define-verifier %verify:standalone-nonterminal
      (syntax-rules (quote)
        ((_ s '(k t) 'term '(name (meta-vars ...) productions ...))
         ($ s ($and '(%verify:nonterminal-name '(k ((name (meta-vars ...)) . t)) 'name)
                    '($every? '(%verify:meta-var-name '(k (name . t))) '(meta-vars ...))
                    '(%verify:nonterminal-production-list '(k (name . t)) '(productions ...))
                    '($every? '(%verify:standalone-production '(k (name . t))) '(productions ...)) )))

        ((_ s '(k t) 'term '(name predicate-name (meta-vars ...) productions ...))
         ($ s ($and '(%verify:nonterminal-name '(k ((name predicate-name (meta-vars ...)) . t)) 'name)
                    '(%verify:nonterminal-predicate-name '(k ((name predicate-name (meta-vars ...)) . t)) 'predicate-name)
                    '($every? '(%verify:meta-var-name '(k (name . t))) '(meta-vars ...))
                    '(%verify:nonterminal-production-list '(k (name . t)) '(productions ...))
                    '($every? '(%verify:standalone-production '(k (name . t))) '(productions ...)) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the nonterminal" (term . t)))) ) )

    ;;;
    ;;;  Nonterminal definitions (addition)
    ;;;

    (define-syntax $can-be:nonterminal-addition?
      (syntax-rules (quote +)
        ((_ s '(+ . added-nonterminals)) ($ s '#t))
        ((_ s  _)                        ($ s '#f)) ) )

    (define-standard-checked-verifier ($is-a:nonterminal-addition? $must-be:nonterminal-addition)
      (syntax-rules (quote +)
        ((_ s '(k t) 'term '(+ added-nonterminals ...))
         ($ s ($and '(%verify:nonterminal-addition-list '(k (term . t)) '(added-nonterminals ...))
                    '($every? '(%verify:standalone-nonterminal '(k t)) '(added-nonterminals ...)) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the nonterminal extension" (term . t)))) ) )

    ;;;
    ;;; Nonterminal definitions (removal)
    ;;;

    (define-syntax $can-be:nonterminal-removal?
      (syntax-rules (quote -)
        ((_ s '(- . removed-nonterminals)) ($ s '#t))
        ((_ s  _)                          ($ s '#f)) ) )

    (define-standard-checked-verifier ($is-a:nonterminal-removal? $must-be:nonterminal-removal)
      (syntax-rules (quote -)
        ((_ s '(k t) 'term '(- removed-nonterminals ...))
         ($ s ($and '(%verify:nonterminal-removal-list '(k (term . t)) '(removed-nonterminals ...))
                    '($every? '(%verify:nonterminal-name/concise-definition '(k t)) '(removed-nonterminals ...)) )))

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
        ((_ s '(k t) 'term '(name (meta-vars ...) productions ...))
         ($ s ($and '(%verify:nonterminal-name '(k ((name (meta-vars ...)) . t)) 'name)
                    '($every? '(%verify:meta-var-name '(k (name . t))) '(meta-vars ...))
                    '($every? '(%verify:standalone-production '(k (name . t))) '(productions ...)) )))

        ((_ s '(k t) 'term '(name predicate-name (meta-vars ...) productions ...))
         ($ s ($and '(%verify:nonterminal-name '(k ((name predicate-name (meta-vars ...)) . t)) 'name)
                    '(%verify:nonterminal-predicate-name '(k ((name predicate-name (meta-vars ...)) . t)) 'predicate-name)
                    '($every? '(%verify:meta-var-name '(k (name . t))) '(meta-vars ...))
                    '($every? '(%verify:standalone-production '(k (name . t))) '(productions ...)) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the nonterminal" (term . t)))) ) )

    ;;;
    ;;; Nonterminal definitions (modification)
    ;;;

    (define-syntax $can-be:nonterminal-modification?
      (syntax-rules (quote !)
        ((_ s '(! . modified-nonterminals)) ($ s '#t))
        ((_ s  _)                           ($ s '#f)) ) )

    (define-standard-checked-verifier ($is-a:nonterminal-modification? $must-be:nonterminal-modification)
      (syntax-rules (quote !)
        ((_ s '(k t) 'term '(! modified-nonterminals ...))
         ($ s ($and '(%verify:nonterminal-modification-list '(k (term . t)) '(modified-nonterminals ...))
                    '($every? '(%verify:nonterminal-modification '(k t)) '(modified-nonterminals ...)) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the nonterminal extension" (term . t)))) ) )

    (define-verifier %verify:nonterminal-modification
      (syntax-rules (quote)
        ((_ s '(k t) 'term '(name (meta-var-modifications ...) production-modifications ...))
         ($ s ($and '(%verify:nonterminal-name '(k ((name (meta-var-modifications ...)) . t)) 'name)
                    '($every? '(%verify:meta-var-modification '(k (name . t))) '(meta-var-modifications ...))
                    '($every? '(%verify:production-modification '(k (name . t))) '(production-modifications ...))
                    '(%verify:meta-vars-or-productions-are-modified '(k (name . t))
                       '(meta-var-modifications ...) '(production-modifications ...) ) )))

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

    (define-syntax $expected-a:nonterminal-definition
      (syntax-rules (quote)
        ((_ s 'lang 'invalid-definition)
         (syntax-error "Invalid syntax of the nonterminal extension" lang invalid-definition)) ) )

    (define-syntax $squash-extension-nonterminal-clauses
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

    ;;;
    ;;; Common verifiers
    ;;;

    (define-verifier/symbol %verify:nonterminal-name
      ("Name of the nonterminal must be an identifier") )

    (define-verifier/symbol %verify:nonterminal-predicate-name
      ("Name of the nonterminal predicate must be an identifier") )

    (define-verifier/nonempty-list %verify:nonterminal-production-list
      ("At least one production should be specified for a nonterminal") )

    (define-verifier/nonempty-list %verify:nonterminal-addition-list
      ("At least one nonterminal should be specified for addition") )

    (define-verifier/nonempty-list %verify:nonterminal-removal-list
      ("At least one nonterminal should be specified for removal") )

    (define-verifier/nonempty-list %verify:nonterminal-modification-list
      ("At least one nonterminal should be specified for modification") )

) )

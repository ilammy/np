(define-library (np lang macros structure-terminals)
  ;;;
  ;;; Structural analysis of terminal definitions (standalone and extension)
  ;;;
  (export  $can-be:standalone-terminal?
             $is-a:standalone-terminal?
          $must-be:standalone-terminal

           $can-be:terminal-addition?
             $is-a:terminal-addition?
          $must-be:terminal-addition

           $can-be:terminal-removal?
             $is-a:terminal-removal?
          $must-be:terminal-removal

           $can-be:terminal-modification?
             $is-a:terminal-modification?
          $must-be:terminal-modification

          $expected-a:terminal-definition

          $squash-extension-terminal-clauses

          $get-terminal-modification-meta-vars
          $set-terminal-modification-meta-vars)

  (import (scheme base)
          (sr ck)
          (sr ck filters)
          (sr ck kernel)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates)
          (np lang macros structure-meta-vars)
          (np lang macros verify-utils))

  (begin

    ;;;
    ;;; Terminal definitions (standalone)
    ;;;

    (define-syntax $can-be:standalone-terminal?
      (syntax-rules (quote)
        ((_ s '(name predicate (meta-vars ...))) ($ s '#t))
        ((_ s '(predicate-name (meta-vars ...))) ($ s '#t))
        ((_ s  _)                                ($ s '#f)) ) )

    (define-standard-checkers %verify:standalone-terminal
      ($is-a:standalone-terminal? $must-be:standalone-terminal) )

    (define-verifier %verify:standalone-terminal
      (syntax-rules (quote)
        ((_ s '(k t) 'term '(name predicate (meta-vars ...)))
         ($ s ($and '(%verify:terminal-name '(k (term . t)) 'name)
                    '(%verify:terminal-meta-var-list '(k (name . t)) '(meta-vars ...))
                    '($every? '(%verify:meta-var-name '(k (name . t))) '(meta-vars ...)) )))

        ((_ s '(k t) 'term '(predicate-name (meta-vars ...)))
         ($ s ($and '(%verify:terminal-predicate-name '(k (term . t)) 'predicate-name)
                    '(%verify:terminal-meta-var-list '(k (predicate-name . t)) '(meta-vars ...))
                    '($every? '(%verify:meta-var-name '(k (predicate-name . t))) '(meta-vars ...)) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the terminal" (term . t)))) ) )

    ;;;
    ;;; Terminal definitions (addition)
    ;;;

    (define-syntax $can-be:terminal-addition?
      (syntax-rules (quote +)
        ((_ s '(+ . added-terminals)) ($ s '#t))
        ((_ s  _)                     ($ s '#f)) ) )

    (define-standard-checked-verifier ($is-a:terminal-addition? $must-be:terminal-addition)
      (syntax-rules (quote +)
        ((_ s '(k t) 'term '(+ added-terminals ...))
         ($ s ($and '(%verify:terminal-addition-list '(k (term . t)) '(added-terminals ...))
                    '($every? '(%verify:standalone-terminal '(k t)) '(added-terminals ...)) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the terminal extension" (term . t)))) ) )

    ;;;
    ;;; Terminal definitions (removal)
    ;;;

    (define-syntax $can-be:terminal-removal?
      (syntax-rules (quote -)
        ((_ s '(- . removed-terminals)) ($ s '#t))
        ((_ s  _)                       ($ s '#f)) ) )

    (define-standard-checked-verifier ($is-a:terminal-removal? $must-be:terminal-removal)
      (syntax-rules (quote -)
        ((_ s '(k t) 'term '(- removed-terminals ...))
         ($ s ($and '(%verify:terminal-removal-list '(k (term . t)) '(removed-terminals ...))
                    '($every? '(%verify:terminal-name/definition '(k t)) '(removed-terminals ...)) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the terminal extension" (term . t)))) ) )

    (define-verifier %verify:terminal-name/definition
      (syntax-rules (quote)
        ((_ s '(k t) 'term '())       ($ s (%verify:standalone-terminal '(k t) 'term)))
        ((_ s '(k t) 'term '(a . d))  ($ s (%verify:standalone-terminal '(k t) 'term)))
        ((_ s '(k t) 'term '#(x ...)) ($ s (%verify:standalone-terminal '(k t) 'term)))
        ((_ s '(k t) 'term  _)        ($ s '#t)) ) )

    ;;;
    ;;; Terminal definitions (modification)
    ;;;

    (define-syntax $can-be:terminal-modification?
      (syntax-rules (quote !)
        ((_ s '(! . modified-terminals)) ($ s '#t))
        ((_ s  _)                        ($ s '#f)) ) )

    (define-standard-checked-verifier ($is-a:terminal-modification? $must-be:terminal-modification)
      (syntax-rules (quote !)
        ((_ s '(k t) 'term '(! modified-terminals ...))
         ($ s ($and '(%verify:terminal-modification-list '(k (term . t)) '(modified-terminals ...))
                    '($every? '(%verify:terminal-modification '(k t)) '(modified-terminals ...)) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the terminal extension" (term . t)))) ) )

    (define-verifier %verify:terminal-modification
      (syntax-rules (quote)
        ((_ s '(k t) 'term '(name (meta-var-modifications ...)))
         ($ s ($and '(%verify:terminal-name '(k (term . t)) 'name)
                    '(%verify:terminal-meta-var-modification-list '(k (name . t)) '(meta-var-modifications ...))
                    '($every? '(%verify:meta-var-modification '(k (name . t))) '(meta-var-modifications ...)) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the terminal modification" (term . t)))) ) )

    ;;;
    ;;; Getters, setters, squashers, etc. (Valid input is assumed everywhere.)
    ;;;

    (define-syntax $expected-a:terminal-definition
      (syntax-rules (quote)
        ((_ s 'lang 'invalid-definition)
         (syntax-error "Invalid syntax of the terminal extension" lang invalid-definition)) ) )

    (define-syntax $squash-extension-terminal-clauses
      (syntax-rules (quote)
        ((_ s 'clauses) ($ s ($concatenate ($map '$cdr 'clauses)))) ) )

    (define-syntax $get-terminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s 'lang '(name meta-var-modification-list)) ($ s 'meta-var-modification-list)) ) )

    (define-syntax $set-terminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s 'lang '(name meta-var-modification-list) 'meta-var-modification-list*)
         ($ s '(name meta-var-modification-list*))) ) )

    ;;;
    ;;; Common verifiers
    ;;;

    (define-verifier/symbol %verify:terminal-name
      ("Name of the terminal must be an identifier") )

    (define-verifier/symbol %verify:terminal-predicate-name
      ("Terminal predicate must be a variable in short form") )

    (define-verifier/nonempty-list %verify:terminal-meta-var-list
      ("At least one meta-variable should be specified for a terminal") )

    (define-verifier/nonempty-list %verify:terminal-addition-list
      ("At least one terminal should be specified for addition") )

    (define-verifier/nonempty-list %verify:terminal-removal-list
      ("At least one terminal should be specified for removal") )

    (define-verifier/nonempty-list %verify:terminal-modification-list
      ("At least one terminal should be specified for modification") )

    (define-verifier/nonempty-list %verify:terminal-meta-var-modification-list
      ("Terminal modification should modify meta-variables") )

) )

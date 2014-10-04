(define-library (np lang impl macros structure-terminals)
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
          (np lang impl macros structure-meta-vars)
          (np lang impl macros verify-utils))

  (begin

    ;;;
    ;;; Terminal definitions (standalone)
    ;;;

    (define-syntax $can-be:standalone-terminal?
      (syntax-rules (quote)
        ((_ s '(name predicate meta-var-list)) ($ s '#t))
        ((_ s '(predicate-name meta-var-list)) ($ s '#t))
        ((_ s  _)                              ($ s '#f)) ) )

    (define-standard-checkers %verify:standalone-terminal
      ($is-a:standalone-terminal? $must-be:standalone-terminal) )

    (define-verifier %verify:standalone-terminal
      (syntax-rules (quote)
        ((_ s '(k t) 'term '(name predicate meta-var-list))
         ($ s ($and '(%verify:terminal-name '(k (term . t)) 'name)
                    '(%verify:terminal-meta-var-list '(k (name . t)) 'meta-var-list)
                    '($every? '(%verify:meta-var-name '(k (name . t))) 'meta-var-list) )))

        ((_ s '(k t) 'term '(predicate-name meta-var-list))
         ($ s ($and '(%verify:terminal-predicate-name '(k (term . t)) 'predicate-name)
                    '(%verify:terminal-meta-var-list '(k (predicate-name . t)) 'meta-var-list)
                    '($every? '(%verify:meta-var-name '(k (predicate-name . t))) 'meta-var-list) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the terminal" (term . t)))) ) )

    ;;;
    ;;; Terminal definitions (addition)
    ;;;

    (define-syntax $can-be:terminal-addition?
      (syntax-rules (quote +)
        ((_ s '(+ . added-terminal-list)) ($ s '#t))
        ((_ s  _)                         ($ s '#f)) ) )

    (define-standard-checked-verifier ($is-a:terminal-addition? $must-be:terminal-addition)
      (syntax-rules (quote +)
        ((_ s '(k t) 'term '(+ . added-terminal-list))
         ($ s ($and '(%verify:terminal-addition-list '(k (term . t)) 'added-terminal-list)
                    '($every? '(%verify:standalone-terminal '(k t)) 'added-terminal-list) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the terminal extension" (term . t)))) ) )

    ;;;
    ;;; Terminal definitions (removal)
    ;;;

    (define-syntax $can-be:terminal-removal?
      (syntax-rules (quote -)
        ((_ s '(- . removed-terminal-list)) ($ s '#t))
        ((_ s  _)                           ($ s '#f)) ) )

    (define-standard-checked-verifier ($is-a:terminal-removal? $must-be:terminal-removal)
      (syntax-rules (quote -)
        ((_ s '(k t) 'term '(- . removed-terminal-list))
         ($ s ($and '(%verify:terminal-removal-list '(k (term . t)) 'removed-terminal-list)
                    '($every? '(%verify:terminal-name/definition '(k t)) 'removed-terminal-list) )))

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
        ((_ s '(! . modified-terminal-list)) ($ s '#t))
        ((_ s  _)                            ($ s '#f)) ) )

    (define-standard-checked-verifier ($is-a:terminal-modification? $must-be:terminal-modification)
      (syntax-rules (quote !)
        ((_ s '(k t) 'term '(! . modified-terminal-list))
         ($ s ($and '(%verify:terminal-modification-list '(k (term . t)) 'modified-terminal-list)
                    '($every? '(%verify:terminal-modification '(k t)) 'modified-terminal-list) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the terminal extension" (term . t)))) ) )

    (define-verifier %verify:terminal-modification
      (syntax-rules (quote)
        ((_ s '(k t) 'term '(name meta-var-modification-list))
         ($ s ($and '(%verify:terminal-name '(k (term . t)) 'name)
                    '(%verify:terminal-meta-var-modification-list '(k (name . t)) 'meta-var-modification-list)
                    '($every? '(%verify:meta-var-modification '(k (name . t))) 'meta-var-modification-list) )))

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

    (define-verifier/atom %verify:terminal-name
      ("Name of the terminal must be a symbol") )

    (define-verifier/atom %verify:terminal-predicate-name
      ("Terminal predicate must be a variable in short form") )

    (define-verifier/proper-list %verify:terminal-definition-list
      ("Unexpected dotted list in terminal definition"
       "Expected terminal definition list") )

    (define-verifier/proper-nonempty-list %verify:terminal-meta-var-list
      ("At least one meta-variable should be specified for a terminal"
       "Unexpected dotted list in terminal definition"
       "Expected a list of meta-variables") )

    (define-verifier/proper-nonempty-list:report-dot-only %verify:terminal-addition-list
      ("At least one terminal should be specified for addition"
       "Unexpected dotted list in terminal extension"
       "Expected a list of terminal definitions") )

    (define-verifier/proper-nonempty-list:report-dot-only %verify:terminal-removal-list
      ("At least one terminal should be specified for removal"
       "Unexpected dotted list in terminal extension"
       "Expected a list of terminal definitions or names") )

    (define-verifier/proper-nonempty-list:report-dot-only %verify:terminal-modification-list
      ("At least one terminal should be specified for modification"
       "Unexpected dotted list in terminal extension"
       "Expected a list of terminal modifications") )

    (define-verifier/proper-nonempty-list %verify:terminal-meta-var-modification-list
      ("Terminal modification should modify meta-variables"
       "Unexpected dotted list in terminal modification"
       "Expected a list of meta-variable modifications") )

) )

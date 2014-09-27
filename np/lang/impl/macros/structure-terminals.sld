(define-library (np lang impl macros structure-terminals)
  ;;;
  ;;; Structural analysis of terminal descriptions (standalone and extension)
  ;;;
  (export  $can-be:standalone-terminal-description?
             $is-a:standalone-terminal-description?
          $must-be:standalone-terminal-description

           $can-be:terminal-addition?
             $is-a:terminal-addition?
          $must-be:terminal-addition

           $can-be:terminal-removal?
             $is-a:terminal-removal?
          $must-be:terminal-removal

           $can-be:terminal-modification?
             $is-a:terminal-modification?
          $must-be:terminal-modification

          $squash-terminal-additions
          $squash-terminal-removals

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
          (np lang impl macros utils))

  (begin

    ;;;
    ;;; Terminal descriptions (standalone)
    ;;;

    (define-syntax $can-be:standalone-terminal-description?
      (syntax-rules (quote)
        ((_ s '(name predicate meta-var-list)) ($ s '#t))
        ((_ s '(predicate-name meta-var-list)) ($ s '#t))
        ((_ s  _)                              ($ s '#f)) ) )

    (define-standard-verifiers ($is-a:standalone-terminal-description?
                                $must-be:standalone-terminal-description
                                : %verify:standalone-terminal-description)
      (syntax-rules (quote)
        ((_ s '(k t) 'term '(name predicate meta-var-list))
         ($ s ($and '(%verify:terminal-name '(k (term . t)) 'name)
                    '(%verify:terminal-meta-var-list '(k (name . t)) 'meta-var-list)
                    '($every? '(%verify:meta-var-name '(k (name . t))) 'meta-var-list) )))

        ((_ s '(k t) 'term '(predicate-name meta-var-list))
         ($ s ($and '(%verify:short-predicate-name '(k (term . t)) 'predicate-name)
                    '(%verify:terminal-meta-var-list '(k (predicate-name . t)) 'meta-var-list)
                    '($every? '(%verify:meta-var-name '(k (predicate-name . t))) 'meta-var-list) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the terminal" (term . t)))) ) )

    (define-syntax %verify:standalone-terminal-description*
      (syntax-rules (quote)
        ((_ s '(k t) 'term) ($ s (%verify:standalone-terminal-description '(k t) 'term 'term))) ) )

    ;;;
    ;;; Terminal descriptions (addition)
    ;;;

    (define-syntax $can-be:terminal-addition?
      (syntax-rules (quote +)
        ((_ s '(+ . anything)) ($ s '#t))
        ((_ s  _)              ($ s '#f)) ) )

    (define-standard-verifiers ($is-a:terminal-addition? $must-be:terminal-addition)
      (syntax-rules (quote +)
        ((_ s '(k t) 'term '(+ . added-terminal-list))
         ($ s ($and '(%verify:terminal-addition-list '(k (term . t)) 'added-terminal-list)
                    '($every? '(%verify:standalone-terminal-description* '(k t)) 'added-terminal-list) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the terminal extension" (term . t)))) ) )

    (define-syntax %verify:terminal-addition-list
      (syntax-rules (quote)
        ((_ s '(k t) '())      ($ k '("At least one terminal should be specified for addition" t)))
        ((_ s '(k t) '(a ...)) ($ s '#t))
        ((_ s '(k t) 'other)   ($ k '("Invalid terminal description syntax" (other . t)))) ) )

    ;;;
    ;;; Terminal descriptions (removal)
    ;;;

    (define-syntax $can-be:terminal-removal?
      (syntax-rules (quote -)
        ((_ s '(- . anything)) ($ s '#t))
        ((_ s  _)              ($ s '#f)) ) )

    (define-standard-verifiers ($is-a:terminal-removal? $must-be:terminal-removal)
      (syntax-rules (quote -)
        ((_ s '(k t) 'term '(- . removed-terminal-list))
         ($ s ($and '(%verify:terminal-removal-list '(k (term . t)) 'removed-terminal-list)
                    '($every? '(%verify:terminal-name/terminal-description '(k t)) 'removed-terminal-list) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the terminal extension" (term . t)))) ) )

    (define-syntax %verify:terminal-name/terminal-description
      (syntax-rules (quote)
        ((_ s '(k t) '())       ($ s (%verify:standalone-terminal-description* '(k t) '())))
        ((_ s '(k t) '(a . d))  ($ s (%verify:standalone-terminal-description* '(k t) '(a . d))))
        ((_ s '(k t) '#(x ...)) ($ s (%verify:standalone-terminal-description* '(k t) '#(x ...))))
        ((_ s '(k t) 'atom)     ($ s '#t)) ) )

    (define-syntax %verify:terminal-removal-list
      (syntax-rules (quote)
        ((_ s '(k t) '())      ($ k '("At least one terminal should be specified for removal" t)))
        ((_ s '(k t) '(a ...)) ($ s '#t))
        ((_ s '(k t) 'other)   ($ k '("Invalid terminal description syntax" (other . t)))) ) )

    ;;;
    ;;; Terminal descriptions (modification)
    ;;;

    (define-syntax $can-be:terminal-modification?
      (syntax-rules (quote !)
        ((_ s '(! . anything)) ($ s '#t))
        ((_ s  _)              ($ s '#f)) ) )

    (define-standard-verifiers ($is-a:terminal-modification? $must-be:terminal-modification)
      (syntax-rules (quote !)
        ((_ s '(k t) 'term '(! . modified-terminal-list))
         ($ s ($and '(%verify:terminal-modification-list '(k (term . t)) 'modified-terminal-list)
                    '($every? '(%verify:terminal-modification* '(k t)) 'modified-terminal-list) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the terminal extension" (term . t)))) ) )

    (define-syntax %verify:terminal-modification-list
      (syntax-rules (quote)
        ((_ s '(k t) '())      ($ k '("At least one terminal should be specified for modification" t)))
        ((_ s '(k t) '(a ...)) ($ s '#t))
        ((_ s '(k t) 'other)   ($ k '("Invalid terminal description syntax" (other . t)))) ) )

    (define-syntax %verify:terminal-modification*
      (syntax-rules (quote)
        ((_ s '(k t) 'term) ($ s (%verify:terminal-modification '(k t) 'term 'term))) ) )

    (define-syntax %verify:terminal-modification
      (syntax-rules (quote)
        ((_ s '(k t) 'term '(name meta-var-modification-list))
         ($ s ($and '(%verify:terminal-name '(k (term . t)) 'name)
                    '(%verify:meta-var-modification-list '(k (name . t)) 'meta-var-modification-list)
                    '($every? '(%verify:meta-var-modification* '(k (name . t))) 'meta-var-modification-list) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the terminal modification" (term . t)))) ) )

    (define-syntax %verify:meta-var-modification-list
      (syntax-rules (quote)
        ((_ s '(k t) '())      ($ k '("Terminal modification should modify meta-variables" t)))
        ((_ s '(k t) '(a ...)) ($ s '#t))
        ((_ s '(k t) 'other)   ($ k '("Expected a list of meta-variable modifications" (other . t)))) ) )

    (define-syntax %verify:meta-var-modification*
      (syntax-rules (quote)
        ((_ s '(k t) 'term) ($ s (%verify:meta-var-modification '(k t) 'term 'term))) ) )

    (define-syntax %verify:meta-var-modification
      (syntax-rules (quote + -)
        ((_ s '(k t) 'term '(+ . meta-var-name-list))
         ($ s ($and '(%verify:terminal-meta-var-list '(k t) 'meta-var-name-list)
                    '($every? '(%verify:meta-var-name '(k t)) 'meta-var-name-list) )))

        ((_ s '(k t) 'term '(- . meta-var-name-list))
         ($ s ($and '(%verify:terminal-meta-var-list '(k t) 'meta-var-name-list)
                    '($every? '(%verify:meta-var-name '(k t)) 'meta-var-name-list) )))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid extension meta-variable syntax" (term . t)))) ) )

    ;;;
    ;;; Getter/setter for meta-vars in terminal modification descriptions
    ;;;

    (define-syntax $get-terminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s 'lang '(name predicate meta-vars)) ($ s 'meta-vars))
        ((_ s 'lang '(predicate-name meta-vars)) ($ s 'meta-vars)) ) )

    (define-syntax $set-terminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s 'lang '(name predicate meta-vars) 'meta-vars*) ($ s '(name predicate meta-vars*)))
        ((_ s 'lang '(predicate-name meta-vars) 'meta-vars*) ($ s '(predicate-name meta-vars*))) ) )

    ;;;
    ;;; Explicit clause squashers (to be applied only to valid clauses)
    ;;;

    (define-syntax $squash-terminal-additions
      (syntax-rules (quote)
        ((_ s 'explicit 'implicit) ($ s ($append ($concatenate ($map '$cdr 'explicit))
                                                 'implicit ))) ) )

    (define-syntax $squash-terminal-removals
      (syntax-rules (quote)
        ((_ s 'removals) ($ s ($concatenate ($map '$cdr 'removals)))) ) )

    ;;;
    ;;; Common matching utilities
    ;;;

    (define-syntax %verify:terminal-name
      (syntax-rules (quote)
        ((_ s '(k t) '())       ($ k '("Name of the terminal must be a symbol" (()       . t))))
        ((_ s '(k t) '(a . d))  ($ k '("Name of the terminal must be a symbol" ((a . d)  . t))))
        ((_ s '(k t) '#(x ...)) ($ k '("Name of the terminal must be a symbol" (#(x ...) . t))))
        ((_ s '(k t)  _)        ($ s '#t)) ) )

    (define-syntax %verify:short-predicate-name
      (syntax-rules (quote)
        ((_ s '(k t) '())       ($ k '("Terminal predicate must be a variable in short form" (()       . t))))
        ((_ s '(k t) '(a . d))  ($ k '("Terminal predicate must be a variable in short form" ((a . d)  . t))))
        ((_ s '(k t) '#(x ...)) ($ k '("Terminal predicate must be a variable in short form" (#(x ...) . t))))
        ((_ s '(k t)  _)        ($ s '#t)) ) )

    (define-syntax %verify:terminal-description-list
      (syntax-rules (quote)
        ((_ s '(k t) '(x ...))     ($ s '#t))
        ((_ s '(k t) '(x ... . a)) ($ k '("Unexpected dotted list in terminal description" (a . t))))
        ((_ s '(k t) 'unexpected)  ($ k '("Expected terminal description list" (unexpected . t)))) ) )

    (define-syntax %verify:terminal-meta-var-list
      (syntax-rules (quote)
        ((_ s '(k t) '())            ($ k '("At least one meta-variable should be specified for a terminal" t)))
        ((_ s '(k t) '(a b ...))     ($ s '#t))
        ((_ s '(k t) '(a b ... . c)) ($ k '("Unexpected dotted list in terminal definition" (c (a b ... . c) . t))))
        ((_ s '(k t) 'unexpected)    ($ k '("Expected a list of meta-variables" (unexpected . t)))) ) )
) )

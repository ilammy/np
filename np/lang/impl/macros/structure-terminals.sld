(define-library (np lang impl macros structure-terminals)
  ;;;
  ;;; Structural analysis of terminal descriptions (standalone and extension)
  ;;;
  (export  $can-be:standalone-terminal-description?
             $is-a:standalone-terminal-description?
          $must-be:standalone-terminal-description

           $can-be:terminal-explicit-addition?
             $is-a:terminal-explicit-addition?
          $must-be:terminal-explicit-addition

           $can-be:terminal-implicit-addition?
             $is-a:terminal-implicit-addition?
          $must-be:terminal-implicit-addition

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
    ;;; Terminal descriptions (standalone) - interface
    ;;;

    (define-syntax $can-be:standalone-terminal-description?
      (syntax-rules (quote)
        ((_ s '(name predicate (meta . vars))) ($ s '#t))
        ((_ s '(predicate      (meta . vars))) ($ s '#t))
        ((_ s  _)                              ($ s '#f)) ) )

    (define-syntax $is-a:standalone-terminal-description?
      (syntax-rules (quote)
        ((_ s 'term) ($ s ($verify-result:as-boolean
                            ($verify:standalone-terminal-description 'term) ))) ) )

    (define-syntax $must-be:standalone-terminal-description
      (syntax-rules (quote)
        ((_ s 'lang 'term) ($ s ($verify-result:syntax-error
                                  ($verify:standalone-terminal-description 'lang 'term) ))) ) )

    ;;;
    ;;; Terminal descriptions (standalone) - implementation
    ;;;

    (define-syntax $verify:standalone-terminal-description
      (syntax-rules (quote)
        ((_ s 'term)       ($ s (%verify:standalone-terminal-description '(s ())     'term)))
        ((_ s 'lang 'term) ($ s (%verify:standalone-terminal-description '(s (lang)) 'term))) ) )

    (define-syntax %verify:standalone-terminal-description
      (syntax-rules (quote)
        ((_ s '(k t) '(name predicate ()))
         ($ s ($and '(%verify:terminal-name '(k ((name predicate ()) . t)) 'name)
                    '(%verify:terminal-meta-var-list '(k ((name predicate ()) . t)) '()) )))

        ((_ s '(k t) '(name predicate (meta . vars)))
         ($ s ($and '(%verify:terminal-name '(k ((name predicate (meta . vars)) . t)) 'name)
                    '(%verify:terminal-meta-var-list '(k (name . t)) '(meta . vars))
                    '($every? '(%verify:meta-var-name '(k (name . t))) '(meta . vars)) )))

        ((_ s '(k t) '(predicate-name ()))
         ($ s ($and '(%verify:short-predicate-name '(k ((predicate-name ()) . t)) 'predicate-name)
                    '(%verify:terminal-meta-var-list '(k ((predicate-name ()) . t)) '()) )))

        ((_ s '(k t) '(predicate-name (meta . vars)))
         ($ s ($and '(%verify:short-predicate-name '(k ((predicate-name (meta . vars)) . t)) 'predicate-name)
                    '(%verify:terminal-meta-var-list '(k (predicate-name . t)) '(meta . vars))
                    '($every? '(%verify:meta-var-name '(k (predicate-name . t))) '(meta . vars)) )))

        ((_ s '(k t) 'invalid-syntax)
         ($ k '("Invalid terminal description syntax" (invalid-syntax . t)))) ) )

    ;;;
    ;;; Terminal descriptions (extension, implicit addition)
    ;;;

    (define-syntax $can-be:terminal-implicit-addition?
      (syntax-rules (quote)
        ((_ s 'term) ($ s ($can-be:standalone-terminal-description? 'term))) ) )

    (define-syntax $is-a:terminal-implicit-addition?
      (syntax-rules (quote)
        ((_ s 'term) ($ s ($is-a:standalone-terminal-description? 'term))) ) )

    (define-syntax $must-be:terminal-implicit-addition
      (syntax-rules (quote)
        ((_ s 'lang 'term) ($ s ($must-be:standalone-terminal-description 'lang 'term))) ) )

    ;;;
    ;;; Terminal descriptions (extension, explicit addition) - interface
    ;;;

    (define-syntax $can-be:terminal-explicit-addition?
      (syntax-rules (quote +)
        ((_ s '(+ . rest)) ($ s '#t))
        ((_ s  _)          ($ s '#f)) ) )

    (define-syntax $is-a:terminal-explicit-addition?
      (syntax-rules (quote)
        ((_ s 'term) ($ s ($verify-result:as-boolean
                            ($verify:terminal-explicit-addition 'term) ))) ) )

    (define-syntax $must-be:terminal-explicit-addition
      (syntax-rules (quote)
        ((_ s 'lang 'term) ($ s ($verify-result:syntax-error
                                  ($verify:terminal-explicit-addition 'lang 'term) ))) ) )

    ;;;
    ;;; Terminal descriptions (extension, implicit addition) - implementation
    ;;;

    (define-syntax $verify:terminal-explicit-addition
      (syntax-rules (quote)
        ((_ s 'term)       ($ s (%verify:terminal-explicit-addition '(s ())     'term)))
        ((_ s 'lang 'term) ($ s (%verify:terminal-explicit-addition '(s (lang)) 'term))) ) )

    (define-syntax %verify:terminal-explicit-addition
      (syntax-rules (quote)
        ((_ s '(k t) '(? . descriptions))
         ($ s ($and '(%verify:terminal-addition-list '(k t) '(? . descriptions) '(? . descriptions))
                    '(%verify:terminal-description-list '(k ((? . descriptions) . t)) 'descriptions)
                    '($every? '(%verify:standalone-terminal-description '(k t)) 'descriptions) )))

        ((_ s '(k t) 'invalid-syntax)
         ($ k '("Invalid terminal description syntax" (invalid-syntax . t)))) ) )

    ;; The 2nd and 3rd arguments are actually the same. This trick is necessary
    ;; to get the (+) form from the original source, because writing just `(+)`
    ;; in the expansion yields a list that contains another plus--that free one
    ;; from (scheme base) as '+' is not a pattern variable here.
    (define-syntax %verify:terminal-addition-list
      (syntax-rules (quote +)
        ((_ s '(k t) '(+)         'x) ($ k '("At least one terminal should be specified for addition" (x . t))))
        ((_ s '(k t) '(+ . other) 'x) ($ s '#t))
        ((_ s '(k t)  _           'x) ($ k '("Invalid terminal description syntax" (x . t)))) ) )

    ;;;
    ;;; Terminal descriptions (extension, removal) - interface
    ;;;

    (define-syntax $can-be:terminal-removal?
      (syntax-rules (quote -)
        ((_ s '(- . rest)) ($ s '#t))
        ((_ s  _)          ($ s '#f)) ) )

    (define-syntax $is-a:terminal-removal?
      (syntax-rules (quote)
        ((_ s 'term) ($ s ($verify-result:as-boolean
                            ($verify:terminal-removal 'term) ))) ) )

    (define-syntax $must-be:terminal-removal
      (syntax-rules (quote)
        ((_ s 'lang 'term) ($ s ($verify-result:syntax-error
                                  ($verify:terminal-removal 'lang 'term) ))) ) )

    ;;;
    ;;; Terminal descriptions (extension, removal) - implementation
    ;;;

    (define-syntax $verify:terminal-removal
      (syntax-rules (quote)
        ((_ s 'term)       ($ s (%verify:terminal-removal '(s ())     'term)))
        ((_ s 'lang 'term) ($ s (%verify:terminal-removal '(s (lang)) 'term))) ) )

    (define-syntax %verify:terminal-removal
      (syntax-rules (quote)
        ((_ s '(k t) '(? . descriptions))
         ($ s ($and '(%verify:terminal-removal-list '(k t) '(? . descriptions) '(? . descriptions))
                    '(%verify:terminal-description-list '(k ((? . descriptions) . t)) 'descriptions)
                    '($every? '(%verify:terminal-name/terminal-description '(k t)) 'descriptions) )))

        ((_ s '(k t) 'invalid-syntax)
         ($ k '("Invalid terminal description syntax" (invalid-syntax . t)))) ) )

    (define-syntax %verify:terminal-name/terminal-description
      (syntax-rules (quote)
        ((_ s '(k t) '())       ($ s (%verify:standalone-terminal-description '(k t) '())))
        ((_ s '(k t) '(a . d))  ($ s (%verify:standalone-terminal-description '(k t) '(a . d))))
        ((_ s '(k t) '#(x ...)) ($ s (%verify:standalone-terminal-description '(k t) '#(x ...))))
        ((_ s '(k t) 'atom)     ($ s '#t)) ) )

    ;; (See comment for %verify:terminal-addition-list)
    (define-syntax %verify:terminal-removal-list
      (syntax-rules (quote -)
        ((_ s '(k t) '(-)         'x) ($ k '("At least one terminal should be specified for removal" (x . t))))
        ((_ s '(k t) '(- . other) 'x) ($ s '#t))
        ((_ s '(k t)  _           'x) ($ k '("Invalid terminal description syntax" (x . t)))) ) )

    ;;;
    ;;; Terminal descriptions (extension, modification) - interface
    ;;;

    ;; A form with predicate is also allowed here as extension meta-variable syntax
    ;; implies a misplaced predicate rather than invalid (standalone) meta-vars
    (define-syntax $can-be:terminal-modification?
      (syntax-rules (quote)
        ((_ s '(name           (vars ...))) ($ s ($any? '$can-be:extension-meta-var? '(vars ...))))
        ((_ s '(name predicate (vars ...))) ($ s ($any? '$can-be:extension-meta-var? '(vars ...))))
        ((_ s _)                            ($ s '#f)) ) )

    (define-syntax $is-a:terminal-modification?
      (syntax-rules (quote)
        ((_ s 'term)
         ($ s ($verify-result:as-boolean ($verify:terminal-modification 'term)))) ) )

    (define-syntax $must-be:terminal-modification
      (syntax-rules (quote)
        ((_ s 'lang 'term)
         ($ s ($verify-result:syntax-error ($verify:terminal-modification 'lang 'term)))) ) )

    ;;;
    ;;; Terminal descriptions (extension, modification) - implementation
    ;;;

    (define-syntax $verify:terminal-modification
      (syntax-rules (quote)
        ((_ s 'term)       ($ s (%verify:terminal-modification '(s ())     'term)))
        ((_ s 'lang 'term) ($ s (%verify:terminal-modification '(s (lang)) 'term))) ) )

    (define-syntax %verify:terminal-modification
      (syntax-rules (quote)
        ((_ s '(k t) '(name ()))
         ($ s ($and '(%verify:terminal-name '(k ((name ()) . t)) 'name)
                    '(%verify:terminal-meta-var-list '(k (name . t)) '()) )))

        ((_ s '(k t) '(name (ext-meta . vars)))
         ($ s ($and '(%verify:terminal-name '(k ((name (ext-meta . vars)) . t)) 'name)
                    '(%verify:terminal-meta-var-list '(k (name . t)) '(ext-meta . vars))
                    '($every? '(%verify:ext-meta-vars '(k (name . t))) '(ext-meta . vars)) )))

        ((_ s '(k t) 'invalid-syntax)
         ($ k '("Invalid terminal description syntax" (invalid-syntax . t)))) ) )

    ;; (See comment for %verify:terminal-addition-list)
    (define-syntax %verify:ext-meta-vars
      (syntax-rules (quote + -)
        ((_ s '(k t) 'x) ($ s (%verify:ext-meta-vars '(k t) 'x 'x)))

        ((_ s '(k t) '(+) 'x)
         ($ k '("At least one meta-variable should be specified for addition" (x . t))))

        ((_ s '(k t) '(-) 'x)
         ($ k '("At least one meta-variable should be specified for removal" (x . t))))

        ((_ s '(k t) '(+ . list) 'x)
         ($ s ($and '(%verify:terminal-meta-var-list '(k t) 'x)
                    '($every? '(%verify:meta-var-name '(k (x . t))) 'list) )))

        ((_ s '(k t) '(- . list) 'x)
         ($ s ($and '(%verify:terminal-meta-var-list '(k t) 'x)
                    '($every? '(%verify:meta-var-name '(k (x . t))) 'list) )))

        ((_ s '(k t) _ 'x) ($ k '("Invalid extension meta-variable syntax" (x . t)))) ) )

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
        ((_ s '(k t) '())       ($ k '("Terminal name must be a symbol" (()       . t))))
        ((_ s '(k t) '(a . d))  ($ k '("Terminal name must be a symbol" ((a . d)  . t))))
        ((_ s '(k t) '#(x ...)) ($ k '("Terminal name must be a symbol" (#(x ...) . t))))
        ((_ s '(k t) 'an-atom)  ($ s '#t)) ) )

    (define-syntax %verify:short-predicate-name
      (syntax-rules (quote)
        ((_ s '(k t) '())       ($ k '("Predicate must be a symbol in short form" (()       . t))))
        ((_ s '(k t) '(a . d))  ($ k '("Predicate must be a symbol in short form" ((a . d)  . t))))
        ((_ s '(k t) '#(x ...)) ($ k '("Predicate must be a symbol in short form" (#(x ...) . t))))
        ((_ s '(k t) 'an-atom)  ($ s '#t)) ) )

    (define-syntax %verify:terminal-description-list
      (syntax-rules (quote)
        ((_ s '(k t) '(x ...))     ($ s '#t))
        ((_ s '(k t) '(x ... . a)) ($ k '("Unexpected dotted list in terminal description" (a . t))))
        ((_ s '(k t) 'unexpected)  ($ k '("Expected terminal description list" (unexpected . t)))) ) )

    (define-syntax %verify:terminal-meta-var-list
      (syntax-rules (quote)
        ((_ s '(k t) '())          ($ k '("Terminal must have at least one meta-variable" t)))
        ((_ s '(k t) '(x ...))     ($ s '#t))
        ((_ s '(k t) '(x ... . a)) ($ k '("Unexpected dotted list in terminal description" (a (x ... . a) . t))))
        ((_ s '(k t) 'unexpected)  ($ k '("Expected meta-variable list" (unexpected . t)))) ) )
) )

(define-library (np lang impl macros partitioning-terminals)
  ;;;
  ;;; Partitioning terminal definition clauses (standalone and extension)
  ;;;
  (export $filter-standalone-terminal-definitions
          $partition-extension-terminal-definitions)

  (import (scheme base)
          (sr ck)
          (sr ck filters)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates)
          (np lang impl macros structure-terminals)
          (np lang impl macros structure-meta-vars)
          (np lang impl macros verify-utils))

  (begin

    ;;;
    ;;; Standalone form
    ;;;

    (define-syntax $filter-standalone-terminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang 'definitions)
         ($ s ($check-for-invalid-terminal-definitions 'lang
                ($partition '$is-a:standalone-terminal?
                            'definitions ) ))) ) )

    (define-syntax $check-for-invalid-terminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang '(all-valid-definitions ())) ($ s 'all-valid-definitions))

        ((_ s 'lang '(_ (invalid-definitions ...)))
         ($ s ($map '($must-be:standalone-terminal 'lang)
                    '(invalid-definitions ...) ))) ) )

    ;;;
    ;;; Extension form
    ;;;

    (define-syntax $partition-extension-terminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang 'definitions)
         ($ s ($postprocess-partitioned-extension-terminal-definitions 'lang
                ($multi-partition '($is-a:terminal-addition?
                                    $is-a:terminal-removal?
                                    $is-a:terminal-modification?)
                  'definitions ) ))) ) )

    (define-syntax $postprocess-partitioned-extension-terminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang '(additions removals modifications ()))
         ($ s ($list
                ($squash-extension-terminal-clauses 'additions)
                ($squash-extension-terminal-clauses 'removals)
                ($map '($partition-terminal-modification-meta-vars 'lang)
                      ($squash-extension-terminal-clauses 'modifications) ) )))

        ((_ s 'lang '(_ _ _ (invalid-definitions ...)))
         ($ s ($report-invalid-extension-terminal-definitions 'lang
                ($multi-partition '($can-be:terminal-addition?
                                    $can-be:terminal-removal?
                                    $can-be:terminal-modification?)
                  '(invalid-definitions ...) ) ))) ) )

    (define-syntax $report-invalid-extension-terminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang '(additions removals modifications incomprehensible))
         ($ s ($and '($every? '($must-be:terminal-addition     'lang) 'additions)
                    '($every? '($must-be:terminal-removal      'lang) 'removals)
                    '($every? '($must-be:terminal-modification 'lang) 'modifications)
                    '($map '($expected-a:terminal-definition   'lang) 'incomprehensible) ))) ) )

    ;;;
    ;;; Partitioning of meta-vars of the modification extension form
    ;;;

    ;; Validity of meta-variables has been already verified by $is-a:terminal-modification?
    ;; so we can assume it and use $can-be? checks here (which are more simple).

    (define-syntax $partition-terminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s 'lang 'terminal-modification)
         ($ s ($set-terminal-modification-meta-vars 'lang 'terminal-modification
                ($squash-terminal-modification-meta-vars
                  ($multi-partition '($can-be:meta-var-addition? $can-be:meta-var-removal?)
                    ($get-terminal-modification-meta-vars 'lang 'terminal-modification) ) ) ))) ) )

    (define-syntax $squash-terminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s '(added removed ()))
         ($ s ($list ($squash-extension-meta-variables 'added)
                     ($squash-extension-meta-variables 'removed) ))) ) )

) )

(define-library (np lang impl macros partitioning-nonterminals)
  ;;;
  ;;; Partitioning nonterminal definition clauses (standalone and extension)
  ;;;
  (export $filter-standalone-nonterminal-definitions
          $partition-extension-nonterminal-definitions)

  (import (scheme base)
          (sr ck)
          (sr ck filters)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates)
          (np lang impl macros structure-nonterminals)
          (np lang impl macros structure-meta-vars)
          (np lang impl macros structure-productions)
          (np lang impl macros verify-utils))

  (begin

    ;;;
    ;;; Standalone form
    ;;;

    (define-syntax $filter-standalone-nonterminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang 'definitions)
         ($ s ($check-for-invalid-nonterminal-definitions 'lang
                ($partition '$is-a:standalone-nonterminal?
                            'definitions ) ))) ) )

    (define-syntax $check-for-invalid-nonterminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang '(all-valid-definitions ())) ($ s 'all-valid-definitions))

        ((_ s 'lang '(_ (invalid-definitions ...)))
         ($ s ($map '($must-be:standalone-nonterminal 'lang)
                    '(invalid-definitions ...) ))) ) )

    ;;;
    ;;; Extension form
    ;;;

    (define-syntax $partition-extension-nonterminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang 'definitions)
         ($ s ($postprocess-partitioned-extension-nonterminal-definitions 'lang
                ($multi-partition '($is-a:nonterminal-addition?
                                    $is-a:nonterminal-removal?
                                    $is-a:nonterminal-modification?)
                  'definitions ) ))) ) )

    (define-syntax $postprocess-partitioned-extension-nonterminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang '(additions removals modifications ()))
         ($ s ($list
                ($squash-extension-nonterminal-clauses 'additions)
                ($squash-extension-nonterminal-clauses 'removals)
                ($map '($partition-nonterminal-modification-productions 'lang)
                  ($map '($partition-nonterminal-modification-meta-vars 'lang)
                        ($squash-extension-nonterminal-clauses 'modifications) ) ) )))

        ((_ s 'lang '(_ _ _ (invalid-definitions ...)))
         ($ s ($report-invalid-extension-nonterminal-definitions 'lang
                ($multi-partition '($can-be:nonterminal-addition?
                                    $can-be:nonterminal-removal?
                                    $can-be:nonterminal-modification?)
                  '(invalid-definitions ...) ) ))) ) )

    (define-syntax $report-invalid-extension-nonterminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang '(additions removals modifications incomprehensible))
         ($ s ($and '($every? '($must-be:nonterminal-addition     'lang) 'additions)
                    '($every? '($must-be:nonterminal-removal      'lang) 'removals)
                    '($every? '($must-be:nonterminal-modification 'lang) 'modifications)
                    '($map '($expected-a:nonterminal-definition   'lang) 'incomprehensible) ))) ) )

    ;;;
    ;;; Partitioning of meta-vars and productions of the modification extension form
    ;;;

    ;; Validity of meta-variables and productions has been already verified by
    ;; $is-a:nonterminal-modification? so we can assume it and use $can-be? checks
    ;; here (which are more simple).

    (define-syntax $partition-nonterminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s 'lang 'nonterminal-modification)
         ($ s ($set-nonterminal-modification-meta-vars 'lang 'nonterminal-modification
                ($squash-nonterminal-modification-meta-vars
                  ($multi-partition '($can-be:meta-var-addition? $can-be:meta-var-removal?)
                    ($get-nonterminal-modification-meta-vars 'lang 'nonterminal-modification) ) ) ))) ) )

    (define-syntax $squash-nonterminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s '(added removed ()))
         ($ s ($list ($squash-extension-meta-variables 'added)
                     ($squash-extension-meta-variables 'removed) ))) ) )

    (define-syntax $partition-nonterminal-modification-productions
      (syntax-rules (quote)
        ((_ s 'lang 'nonterminal-modification)
         ($ s ($set-nonterminal-modification-productions 'lang 'nonterminal-modification
                ($squash-nonterminal-modification-productions
                  ($multi-partition '($can-be:production-addition? $can-be:production-removal?)
                    ($get-nonterminal-modification-productions 'lang 'nonterminal-modification) ) ) ))) ) )

    (define-syntax $squash-nonterminal-modification-productions
      (syntax-rules (quote)
        ((_ s '(added removed ()))
         ($ s ($list ($squash-extension-productions 'added)
                     ($squash-extension-productions 'removed) ))) ) )

) )

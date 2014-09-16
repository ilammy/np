(define-library (np lang impl macros partitioning-terminals)
  ;;;
  ;;; Partitioning terminal description clauses (standalone and extension)
  ;;;
  (export $filter-standalone-terminal-descriptions
          $partition-extension-terminal-descriptions)

  (import (scheme base)                   (sr ck predicates)
          (sr ck)
          (sr ck filters)
          (sr ck lists)
          (sr ck maps)
          (np lang impl macros structure-terminals)
          (np lang impl macros structure-meta-vars)
          (np lang impl macros utils))

  (begin
    ;;;
    ;;; Standalone form
    ;;;

    (define-syntax $filter-standalone-terminal-descriptions
      (syntax-rules (quote)
        ((_ s 'lang 'descriptions)
         ($ s ($check-for-invalid-terminal-descriptions 'lang
                ($partition '$is-a:standalone-terminal-description?
                            'descriptions ) )))  ) )

    (define-syntax $check-for-invalid-terminal-descriptions
      (syntax-rules (quote)
        ((_ s 'lang '(valid-descriptions ())) ($ s 'valid-descriptions))

        ((_ s 'lang '(_ (invalid-descriptions ...)))
         ($ s ($map '($must-be:standalone-terminal-description 'lang)
                    '(invalid-descriptions ...) ))) ) )

    ;;;
    ;;; Extension form
    ;;;

    (define-syntax $partition-extension-terminal-descriptions
      (syntax-rules (quote)
        ((_ s 'lang 'descriptions)
         ($ s ($cleanup-partitioned-extension-terminal-descriptions 'lang
                ($multi-partition '($is-a:terminal-explicit-addition?
                                    $is-a:terminal-implicit-addition?
                                    $is-a:terminal-removal?
                                    $is-a:terminal-modification?)
                  'descriptions ) ))) ) )

    (define-syntax $cleanup-partitioned-extension-terminal-descriptions
      (syntax-rules (quote)
        ((_ s 'lang '(explicit-pluses implicit-pluses minuses edits ()))
         ($ s ($list 'explicit-pluses 'implicit-pluses 'minuses 'edits)))       ; incorrect, needs postprocessing

        ((_ s 'lang '(_ _ _ _ (invalid-descriptions ...)))
         ($ s ($report-invalid-extension-terminal-descriptions 'lang
                ($multi-partition '($can-be:terminal-explicit-addition?
                                    $can-be:terminal-removal?
                                    $can-be:terminal-modification?
                                    $can-be:terminal-implicit-addition?)
                  '(invalid-descriptions ...) ) ))) ) )

    ;; $can-be checks go in such order because they are used to heuristically
    ;; guess an implied class for a structurally invalid clauses. The $multi-
    ;; partition picks the first predicate that returns #t, therefore at first
    ;; we check for the clauses with distinct features: explicit additions,
    ;; removals, and modifications have (+ ...) / (- ...) clauses that identify
    ;; them pretty well. Anything else is considered a failed implicit addition

    (define-syntax $report-invalid-extension-terminal-descriptions
      (syntax-rules (quote)
        ((_ s 'lang '(explicit-pluses minuses edits implicit-pluses incomprehensible))
         ($ s ($and '($every? '($must-be:terminal-explicit-addition 'lang) 'explicit-pluses)
                    '($every? '($must-be:terminal-removal           'lang) 'minuses)
                    '($every? '($must-be:terminal-modification      'lang) 'edits)
                    '($every? '($must-be:terminal-implicit-addition 'lang) 'implicit-pluses)
                    '($every? '($must-be:terminal-implicit-addition 'lang) 'incomprehensible) ))) ) )

    ;;;
    ;;; Partitioning of meta-vars of the modification extension form
    ;;;

    (define-syntax $partition-terminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s 'lang 'terminal-modification)
         ($ s ($set-terminal-modification-meta-vars 'lang 'terminal-modification
                ($cleanup-terminal-modification-meta-vars 'lang 'terminal-modification
                  ($multi-partition '($can-be:meta-var-addition?
                                      $can-be:meta-var-removal?)
                    ($get-terminal-modification-meta-vars 'lang 'terminal-modification) ) ) ))) ) )

    (define-syntax $cleanup-terminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s _ _ '(added removed ()))
         ($ s ($list ($drop-head-and-squash 'added)
                     ($drop-head-and-squash 'removed) )))

        ((_ s 'lang 'clause '(_ _ (x xs ...)))
         (syntax-error "Invalid meta-var descriptions" lang clause (x xs ...))) ) )

) )

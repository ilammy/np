(define-library (np lang impl macros partitioning-nonterminals)
  ;;;
  ;;; Partitioning nonterminal definition clauses (standalone and extension)
  ;;;
  (export $filter-standalone-nonterminal-descriptions
          $partition-extension-nonterminal-descriptions)

  (import (scheme base)
          (sr ck)
          (sr ck filters)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates)
          (np lang impl macros structure-nonterminals)
          (np lang impl macros structure-meta-vars)
          (np lang impl macros utils))

  (begin
    ;;;
    ;;; Standalone form
    ;;;

    (define-syntax $filter-standalone-nonterminal-descriptions
      (syntax-rules (quote)
        ((_ s 'lang 'descriptions)
         ($ s ($check-for-invalid-nonterminal-descriptions 'lang
                ($partition '$is-a:standalone-nonterminal?
                            'descriptions ) ))) ) )

    (define-syntax $check-for-invalid-nonterminal-descriptions
      (syntax-rules (quote)
        ((_ s 'lang '(all-valid-descriptions ())) ($ s 'all-valid-descriptions))

        ((_ s 'lang '(_ (invalid-descriptions ...)))
         ($ s ($map '($must-be:standalone-nonterminal 'lang)
                    '(invalid-descriptions ...) ))) ) )

    ;;;
    ;;; Extension form
    ;;;

    (define-syntax $partition-extension-nonterminal-descriptions
      (syntax-rules (quote)
        ((_ s 'lang 'descriptions)
         ($ s ($postprocess-partitioned-extension-nonterminal-descriptions 'lang
                ($multi-partition '($is-a:nonterminal-explicit-addition?
                                    $is-a:nonterminal-implicit-addition?
                                    $is-a:nonterminal-removal?
                                    $is-a:nonterminal-modification?)
                  'descriptions ) ))) ) )

    (define-syntax $postprocess-partitioned-extension-nonterminal-descriptions
      (syntax-rules (quote)
        ((_ s 'lang '(explicit-additions implicit-additions removals modifications ()))
         ($ s '(explicit-additions implicit-additions removals modifications)))

        ((_ s 'lang '(_ _ _ _ (invalid-descriptions ...)))
         ($ s ($report-invalid-extension-nonterminal-descriptions 'lang
                ($multi-partition '($can-be:nonterminal-explicit-addition?
                                    $can-be:nonterminal-removal?
                                    $can-be:nonterminal-modification?
                                    $can-be:nonterminal-implicit-addition?)
                  '(invalid-descriptions ...) ) ))) ) )

    ;; $can-be checks go in such order because they are used to heuristically
    ;; guess an implied class for a structurally invalid clauses. The $multi-
    ;; partition picks the first predicate that returns #t, therefore at first
    ;; we check for the clauses with distinct features: explicit additions,
    ;; removals, and modifications have (+ ...) / (- ...) clauses that identify
    ;; them pretty well. Anything else is considered a failed implicit addition

    (define-syntax $report-invalid-extension-nonterminal-descriptions
      (syntax-rules (quote)
        ((_ s 'lang '(explicit-additions removals modifications implicit-additions incomprehensible))
         ($ s ($and '($every? '($must-be:nonterminal-explicit-addition 'lang) 'explicit-additions)
                    '($every? '($must-be:nonterminal-removal           'lang) 'removals)
                    '($every? '($must-be:nonterminal-modification      'lang) 'modifications)
                    '($every? '($must-be:nonterminal-implicit-addition 'lang) 'implicit-additions)
                    '($every? '($must-be:nonterminal-implicit-addition 'lang) 'incomprehensible) ))) ) )

    ;;;
    ;;; Partitioning of meta-vars of the modification extension form
    ;;;

    (define-syntax $partition-nonterminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s 'lang 'nonterminal-modification)
         ($ s ($set-nonterminal-modification-meta-vars 'lang 'nonterminal-modification
                ($cleanup-nonterminal-modification-meta-vars 'lang 'nonterminal-modification
                  ($multi-partition '($can-be:meta-var-addition?
                                      $can-be:meta-var-removal?)
                    ($get-nonterminal-modification-meta-vars 'lang 'nonterminal-modification) ) ) ))) ) )

    (define-syntax $cleanup-nonterminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s _ _ '(added removed ()))
         ($ s ($list ($drop-head-and-squash 'added)
                     ($drop-head-and-squash 'removed) )))

        ((_ s 'lang 'clause '(_ _ (x xs ...)))
         (syntax-error "Invalid meta-var descriptions" lang clause (x xs ...))) ) )

    ;;;
    ;;; Partitioning of productions of the modification extension form
    ;;;

    (define-syntax $partition-nonterminal-modification-productions
      (syntax-rules (quote)
        ((_ s 'lang 'nonterminal-modification)
         ($ s ($set-nonterminal-modification-productions 'lang 'nonterminal-modification
                ($cleanup-nonterminal-modification-productions 'lang 'nonterminal-modification
                  ($multi-partition '($can-be:production-addition?
                                      $can-be:production-removal?)
                    ($get-nonterminal-modification-productions 'lang 'nonterminal-modification) ) ) ))) ) )

    (define-syntax $cleanup-nonterminal-modification-productions
      (syntax-rules (quote)
        ((_ s _ _ '(added removed ()))
         ($ s ($list ($drop-head-and-squash 'added)
                     ($drop-head-and-squash 'removed) )))

        ((_ s 'lang 'clause '(_ _ (x xs ...)))
         (syntax-error "Invalid meta-var descriptions" lang clause (x xs ...))) ) )

) )

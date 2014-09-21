(define-library (np lang impl macros partitioning-nonterminals)
  ;;;
  ;;; Partitioning nonterminal description clauses (standalone and extension)
  ;;;
  (export $filter-standalone-nonterminal-descriptions
          $partition-extension-nonterminal-descriptions)

  (import (scheme base)
          (sr ck)
          (sr ck filters)
          (sr ck lists)
          (sr ck maps)
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
                ($partition '$is-a:standalone-nonterminal-description?
                            'descriptions ) ))) ) )

    (define-syntax $check-for-invalid-nonterminal-descriptions
      (syntax-rules (quote)
        ((_ s 'lang '(all-valid-descriptions ())) ($ s 'all-valid-descriptions))

        ((_ s 'lang '(_ (invalid-descriptions ...)))
         ($ s ($map '($must-be:standalone-nonterminal-description 'lang)
                    '(invalid-descriptions ...) ))) ) )

    ;;;
    ;;; Extension form
    ;;;

    (define-syntax $partition-extension-nonterminal-descriptions
      (syntax-rules (quote)
        ((_ s 'lang 'descriptions)
         ($ s ($cleanup-partitioned-extension-nonterminal-descriptions 'lang
                ($multi-partition '($can-be:nonterminal-modification?
                                    $can-be:nonterminal-removal?
                                    $can-be:nonterminal-explicit-addition?
                                    $can-be:nonterminal-implicit-addition?)
                  'descriptions ) ))) ) )

    (define-syntax $cleanup-partitioned-extension-nonterminal-descriptions
      (syntax-rules (quote)
        ((_ s 'lang '(edits minuses explicit-pluses implicit-pluses ()))
         ($ s ($list
                ($map '($must-be:nonterminal-description-addition 'lang)
                      ($append ($drop-head-and-squash 'explicit-pluses)
                               'implicit-pluses ) )

                ($map '($must-be:nonterminal-description-removal 'lang)
                      ($drop-head-and-squash 'minuses) )

                ($map '($must-be:nonterminal-description-modification 'lang)
                  ($map '($partition-nonterminal-modification-productions 'lang)
                    ($map '($partition-nonterminal-modification-meta-vars 'lang)
                          'edits ) ) ) )))

        ((_ s 'lang '(_ _ _ _ (x xs ...)))
         (syntax-error "Invalid nonterminal description syntax" lang x xs ...)) ) )

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

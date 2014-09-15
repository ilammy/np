(define-library (np lang impl macros partitioning-terminals)
  ;;;
  ;;; Partitioning terminal description clauses (standalone and extension)
  ;;;
  (export $filter-standalone-terminal-descriptions
          $partition-extension-terminal-descriptions)

  (import (scheme base)
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

    ;; Modification is checked first, because some valid modification clauses
    ;; are also considered valid by $can-be:terminal-removal?, but obviously
    ;; fail the $must-be:terminal-description-removal verification. A valid
    ;; removal clause is never a valid modification one, so it's okay to do so.
    ;;
    ;; Implicit addition is checked last to avoid false positives from it.
    ;;
    (define-syntax $partition-extension-terminal-descriptions
      (syntax-rules (quote)
        ((_ s 'lang 'descriptions)
         ($ s ($cleanup-partitioned-extension-terminal-descriptions 'lang
                ($multi-partition '($can-be:terminal-modification?
                                    $can-be:terminal-removal?
                                    $can-be:terminal-explicit-addition?
                                    $can-be:terminal-implicit-addition?)
                  'descriptions ) ))) ) )

    (define-syntax $cleanup-partitioned-extension-terminal-descriptions
      (syntax-rules (quote)
        ((_ s 'lang '(edits minuses explicit-pluses implicit-pluses ()))
         ($ s ($list
                ($map '($must-be:terminal-description-addition 'lang)
                      ($append ($drop-head-and-squash
                                 ($map '($must-be:proper-list 'lang)
                                       'explicit-pluses ) )
                               'implicit-pluses ) )

                ($map '($must-be:terminal-description-removal 'lang)
                      ($drop-head-and-squash
                        ($map '($must-be:proper-list 'lang)
                              'minuses ) ) )

                ($map '($must-be:terminal-description-modification 'lang)
                  ($map '($partition-terminal-modification-meta-vars 'lang)
                        'edits ) ) )))

        ((_ s 'lang '(_ _ _ _ (x xs ...)))
         (syntax-error "Invalid terminal description syntax" lang x xs ...)) ) )

    (define-syntax $must-be:proper-list
      (syntax-rules (quote)
        ((_ s 'lang '(list ...)) ($ s '(list ...)))
        ((_ s 'lang '(list ... . stray-atom))
         (syntax-error "Unexpected dotted list in terminal description" lang
                       (list ... . stray-atom) stray-atom)) ) )

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

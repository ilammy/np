(define-library (np lang impl macros partitioning-nonterminals)
  ;;;
  ;;; Partitioning nonterminal description clauses (standalone and extension)
  ;;;
  (export $filter-standalone-nonterminal-descriptions)

  (import (scheme base)
          (sr ck)
          (sr ck filters)
          (sr ck lists)
          (sr ck maps)
          (np lang impl macros structure-nonterminals))

  (begin
    ;;;
    ;;; Standalone form
    ;;;

    (define-syntax $filter-standalone-nonterminal-descriptions
      (syntax-rules (quote)
        ((_ s 'lang 'descriptions)
         ($ s ($cleanup-partitioned-standalone-nonterminal-descriptions 'lang
                ($partition '$can-be:standalone-nonterminal-description?
                            'descriptions ) ))) ) )

    (define-syntax $cleanup-partitioned-standalone-nonterminal-descriptions
      (syntax-rules (quote)
        ((_ s 'lang '(possible-descriptions ()))
         ($ s ($map '($must-be:standalone-nonterminal-description 'lang)
                    'possible-descriptions )))

        ((_ s 'lang '(_ (x xs ...)))
         (syntax-error "Invalid nonterminal description syntax" x xs ...)) ) )

) )

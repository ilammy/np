(define-library (np lang impl macros partitioning)
  ;;;
  ;;; Macrofunctions that partition, group, and regroup expressions into
  ;;; various classes based on their structural features.
  ;;;
  (export $partition-toplevel-clauses
    )

  (import (scheme base)
          (sr ck)
          (sr ck filters)
          (sr ck lists)
          (sr ck maps)
          (np lang impl macros structure))

  (begin
    ;;;
    ;;; Preliminary partitioning
    ;;;

    (define-syntax $partition-toplevel-clauses
      (syntax-rules (quote)
        ((_ s 'lang 'clauses)
         ($ s ($cleanup-partitioned-toplevel-clauses 'lang
                ($multi-partition '($can-be:extends-clause?
                                    $can-be:predicate-clause?
                                    $can-be:parser-clause?
                                    $can-be:unparser-clause?
                                    $can-be:terminals-clause?
                                    $can-be:nonterminal-clause?)
                  'clauses ) ))) ) )

    (define-syntax $cleanup-partitioned-toplevel-clauses
      (syntax-rules (quote)
        ((_ s 'lang '(_ _ _ _ _ _ (x xs ...)))
         (syntax-error "Invalid language description syntax" lang x xs ...))
        ((_ s 'lang '(e r p u t n ()))
         ($ s ($list
                ($at-most-one 'e '(lang "Only one 'extends' clause allowed"))
                ($at-most-one 'r '(lang "Only one 'predicate' clause allowed"))
                ($at-most-one 'p '(lang "Only one 'parser' clause allowed"))
                ($at-most-one 'u '(lang "Only one 'unparser' clause allowed"))
                ($squash-terminals 't)
                'n ))) ) )

    (define-syntax $at-most-one
      (syntax-rules (quote)
        ((_ s '() _)  ($ s '#f))
        ((_ s '(x) _) ($ s 'x))
        ((_ s '(x xs ...) '(lang msg))
         (syntax-error msg lang)) ) )

    (define-syntax $squash-terminals
      (syntax-rules (quote)
        ((_ s 'terminals)
         ($ s ($concatenate ($map '$cdr 'terminals)))) ) )

) )

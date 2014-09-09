(define-library (np lang impl macros utils)
  ;;;
  ;;; Miscellaneous utility macrofunctions that are used here and there,
  ;;; but lack semantic specificity to be placed elsewhere.
  ;;;
  (export $not-vector-or-list?
          $drop-head-and-squash)

  (import (scheme base)
          (sr ck)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates))

  (begin

    (define-syntax $not-vector-or-list?
      (syntax-rules (quote)
        ((_ s '(things ...)) ($ s '#f))
        ((_ s '#(stuff ...)) ($ s '#f))
        ((_ s 'atom)         ($ s '#t)) ) )

    (define-syntax $drop-head-and-squash
      (syntax-rules (quote)
        ((_ s 'list)
         ($ s ($concatenate ($map '$cdr 'list)))) ) )

) )

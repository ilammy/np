(define-library (np lang impl macros utils)
  ;;;
  ;;; Miscellaneous utility macrofunctions that are used here and there,
  ;;; but lack semantic specificity to be placed elsewhere.
  ;;;
  (export $not-vector-or-list?
          $drop-head-and-squash
          $verify-result:as-boolean
          $verify-result:syntax-error
          define-standard-verifiers)

  (import (scheme base)
          (sr ck)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates))

  (begin

    (define-syntax $not-vector-or-list?
      (syntax-rules (quote)
        ((_ s '())           ($ s '#f))
        ((_ s '(car . cdr))  ($ s '#f))
        ((_ s '#(stuff ...)) ($ s '#f))
        ((_ s 'atom)         ($ s '#t)) ) )

    (define-syntax $drop-head-and-squash
      (syntax-rules (quote)
        ((_ s 'list)
         ($ s ($concatenate ($map '$cdr 'list)))) ) )

    (define-syntax $verify-result:as-boolean
      (syntax-rules (quote)
        ((_ s '#t) ($ s '#t))
        ((_ s '::) ($ s '#f)) ) )

    (define-syntax $verify-result:syntax-error
      (syntax-rules (quote)
        ((_ s '#t) ($ s '#t))
        ((_ s '(msg stack)) ($ s ($verify-result:syntax-error '#f 'msg ($reverse 'stack))))
        ((_ s '#f 'msg '(stack ...)) (syntax-error msg stack ...)) ) )

    ;; This helper macro defines a pair of standard verification macrofunctions:
    ;; $is-a:<something> and $must-be:<something> which are implemented in terms
    ;; of a %verification macrofunction that is defined inline.
    (define-syntax define-standard-verifiers
      (syntax-rules (:)
        ((_ ($is-a? $must-be) . %verify-body)
         (define-standard-verifiers ($is-a? $must-be : %verify) . %verify-body))

        ((_ ($is-a? $must-be : %verify) . %verify-body)
         (begin
           (define-syntax $is-a?
             (syntax-rules (quote)
               ((_ s 'term)
                ($ s ($verify-result:as-boolean ($trampoline 'term)))) ) )

           (define-syntax $must-be
             (syntax-rules (quote)
               ((_ s 'lang 'term)
                ($ s ($verify-result:syntax-error ($trampoline 'lang 'term)))) ) )

           (define-syntax $trampoline
             (syntax-rules (quote)
               ((_ s 'term)       ($ s (%verify '(s ())     'term 'term)))
               ((_ s 'lang 'term) ($ s (%verify '(s (lang)) 'term 'term))) ) )

           (define-syntax %verify
             . %verify-body ) )) ) )

) )

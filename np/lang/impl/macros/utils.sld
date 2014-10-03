(define-library (np lang impl macros utils)
  ;;;
  ;;; Miscellaneous utility macrofunctions that are used here and there,
  ;;; but lack semantic specificity to be placed elsewhere.
  ;;;
  (export $not-vector-or-list?
          $drop-head-and-squash

          $verify-result:as-boolean
          $verify-result:syntax-error

          define-standard-checkers
          define-standard-checked-verifier

          define-verifier
          define-verifier/atom
          define-verifier/proper-list
          define-verifier/proper-nonempty-list
          define-verifier/proper-nonempty-list:report-dot-only)

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

    ;;;
    ;;; Verification landing pads
    ;;;

    (define-syntax $verify-result:as-boolean
      (syntax-rules (quote)
        ((_ s '#t) ($ s '#t))
        ((_ s '::) ($ s '#f)) ) )

    (define-syntax $verify-result:syntax-error
      (syntax-rules (quote)
        ((_ s '#t) ($ s '#t))
        ((_ s '(msg stack)) ($ s ($verify-result:syntax-error '#f 'msg ($reverse 'stack))))
        ((_ s '#f 'msg '(stack ...)) (syntax-error msg stack ...)) ) )

    ;;;
    ;;; Verifier declarations
    ;;;

    (define-syntax define-verifier
      (syntax-rules ()
        ((_ %verify . body)
         (begin
           (define-syntax %verify
             (syntax-rules (quote)
               ((_ s '(k t) 'term) ($ s (%verify* '(k t) 'term 'term))) ) )

           (define-syntax %verify*
             . body ) )) ) )

    (define-syntax define-standard-checkers
      (syntax-rules ()
        ((_ %verify ($is-a? $must-be))
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
               ((_ s 'term)       ($ s (%verify '(s ())     'term)))
               ((_ s 'lang 'term) ($ s (%verify '(s (lang)) 'term))) ) ) )) ) )

    (define-syntax define-standard-checked-verifier
      (syntax-rules ()
        ((_ ($is-a? $must-be) . verifier-body)
         (begin
           (define-verifier          %verify . verifier-body)
           (define-standard-checkers %verify ($is-a? $must-be)) )) ) )

    ;;;
    ;;; Common verifiers
    ;;;

    (define-syntax define-verifier/atom
      (syntax-rules ::: ()
        ((_ %verify (:not-atom-error-message))
         (define-verifier %verify
           (syntax-rules (quote)
             ((_ s '(k t) 'term '())       ($ k '(:not-atom-error-message (term . t))))
             ((_ s '(k t) 'term '(a . d))  ($ k '(:not-atom-error-message (term . t))))
             ((_ s '(k t) 'term '#(x ...)) ($ k '(:not-atom-error-message (term . t))))
             ((_ s '(k t) 'term  _)        ($ s '#t)) ) )) ) )

    (define-syntax define-verifier/proper-list
      (syntax-rules ::: ()
        ((_ %verify (:proper-error-message :unexpected-error-message))
         (define-verifier %verify
           (syntax-rules (quote)
             ((_ s '(k t) 'term '(x ...))     ($ s '#t))
             ((_ s '(k t) 'term '(x ... . d)) ($ k '(:proper-error-message (d term . t))))
             ((_ s '(k t) 'term  _)           ($ k '(:unexpected-error-message (term . t)))) ) )) ) )

    (define-syntax define-verifier/proper-nonempty-list
      (syntax-rules ::: ()
        ((_ %verify (:nonempty-error-message :proper-error-message :unexpected-error-message))
         (define-verifier %verify
           (syntax-rules (quote)
             ((_ s '(k t) 'term '())            ($ k '(:nonempty-error-message t)))
             ((_ s '(k t) 'term '(x y ...))     ($ s '#t))
             ((_ s '(k t) 'term '(x y ... . d)) ($ k '(:proper-error-message (d term . t))))
             ((_ s '(k t) 'term  _)             ($ k '(:unexpected-error-message (term . t)))) ) )) ) )

    (define-syntax define-verifier/proper-nonempty-list:report-dot-only
      (syntax-rules ::: ()
        ((_ %verify (:nonempty-error-message :proper-error-message :unexpected-error-message))
         (define-verifier %verify
           (syntax-rules (quote)
             ((_ s '(k t) 'term '())            ($ k '(:nonempty-error-message t)))
             ((_ s '(k t) 'term '(x y ...))     ($ s '#t))
             ((_ s '(k t) 'term '(x y ... . d)) ($ k '(:proper-error-message (d . t))))
             ((_ s '(k t) 'term  _)             ($ k '(:unexpected-error-message (term . t)))) ) )) ) )

) )

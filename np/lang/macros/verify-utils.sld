(define-library (np lang macros verify-utils)
  ;;;
  ;;; Utilities for declaring %verification macrofunctions.
  ;;;
  (export define-standard-checkers
          define-toplevel-checkers
          define-standard-checked-verifier

          define-verifier
          define-verifier/symbol
          define-verifier/proper-list
          define-verifier/proper-nonempty-list
          define-verifier/proper-nonempty-list:report-dot-only)

  (import (scheme base)
          (sr ck)
          (sr ck kernel)
          (sr ck lists)
          (sr ck predicates))

  (begin

    ;;;
    ;;; Verification landing pads
    ;;;

    (define-syntax $verify-result:as-boolean
      (syntax-rules (quote)
        ((_ s '#t) ($ s '#t))
        ((_ s  _)  ($ s '#f)) ) )

    (define-syntax $verify-result:syntax-error
      (syntax-rules (quote)
        ((_ s '#t)                   ($ s '#t))
        ((_ s '(msg stack))          ($ s ($verify-result:syntax-error '#f 'msg ($reverse 'stack))))
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

    (define-syntax define-toplevel-checkers
      (syntax-rules ()
        ((_ %verify ($is-a? $must-be))
         (begin
           (define-syntax $is-a?
             (syntax-rules (quote)
               ((_ s 'term)
                ($ s ($verify-result:as-boolean ($trampoline 'term)))) ) )

           (define-syntax $must-be
             (syntax-rules (quote)
               ((_ s 'term)
                ($ s ($verify-result:syntax-error ($trampoline 'term)))) ) )

           (define-syntax $trampoline
             (syntax-rules (quote)
               ((_ s 'term) ($ s (%verify '(s ()) 'term))) ) ) )) ) )

    (define-syntax define-standard-checked-verifier
      (syntax-rules ()
        ((_ ($is-a? $must-be) . verifier-body)
         (begin
           (define-verifier          %verify . verifier-body)
           (define-standard-checkers %verify ($is-a? $must-be)) )) ) )

    ;;;
    ;;; Common verifiers
    ;;;

    ;; This one is a bit special as it does not pattern-match term structure
    (define-syntax define-verifier/symbol
      (syntax-rules ()
        ((_ %verify (:not-symbol-error-message))
         (begin
           (define-syntax %verify
             (syntax-rules (quote)
               ((_ s '(k t) 'term) ($ s (%verify* '(k t) 'term ($symbol? 'term)))) ) )

           (define-syntax %verify*
             (syntax-rules (quote)
               ((_ s '(k t) 'term '#t) ($ s '#t))
               ((_ s '(k t) 'term '#f) ($ k '(:not-symbol-error-message (term . t)))) ) ) )) ) )

    (define-syntax define-verifier/proper-list
      (syntax-rules ::: ()
        ((_ %verify (:proper-error-message :unexpected-error-message))
         (define-verifier %verify
           (syntax-rules (quote)
             ((_ s '(k t) 'term '(x ...))       ($ s '#t))
             ((_ s '(k t) 'term '(x y ... . d)) ($ k '(:proper-error-message (d term . t))))
             ((_ s '(k t) 'term  _)             ($ k '(:unexpected-error-message (term . t)))) ) )) ) )

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

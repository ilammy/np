(define-library (np lang macros test utils)
  ;;;
  ;;; Utilities for macroexpansion testing.
  ;;;
  (import (scheme base)
          (te conditions define-assertion)
          (only (te conditions builtin-conditions) fail))

  (cond-expand
   (chibi
    (export assert-syntax-error)

    (import (only (chibi) sc-macro-transformer eval))

    (begin
      ;;;
      ;;; Syntax error assertion
      ;;;

      (define no-error-object (list 'no-errors))

      ;; This magic taps into Chibi's macroexpander and intercepts error object
      ;; produced by syntax-error form. Currently, this is regular error-object?
      ;; Unfortunately, it also intercepts any errors resulting from evaluation
      ;; of the expanded form. Let's hope that they cannot be confused.
      (define-syntax catch
        (sc-macro-transformer
         (lambda (form environment)
           (guard (e (#t (list 'quote e)))
             (eval (cadr form) environment)
             (raise no-error-object) ) ) ) )

      (define-assertion (assert-error-equal error message irritants)
        (cond ((not (error-object? error))
               (assert-failure "Not error object" error))
              ((not (string=? message (error-object-message error)))
               (assert-failure "Incorrect error message" (error-object-message error)))
              ((not (equal? irritants (error-object-irritants error)))
               (assert-failure "Incorrect error irritants" (error-object-irritants error)))
              (else (assert-success)) ) )

      (define-syntax assert-syntax-error
        (syntax-rules ()
          ((_ (message . irritants) . body)
           (let ((result (catch . body)))
             (if (eq? result no-error-object)
                 (fail "No errors produced")
                 (assert-error-equal result message 'irritants) ) )) ) )
    ) )
) )

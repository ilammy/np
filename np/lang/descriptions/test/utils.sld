(define-library (np lang descriptions test utils)
  ;;;
  ;;; Utilities for description processing testing.
  ;;;
  (export assert-lang-error)

  (import (scheme base)
          (np lang descriptions definitions)
          (np lang descriptions errors)
          (te conditions define-assertion))

  (begin

    (define-assertion (assert-lang-error kind object)
      (cond ((not (lang-error? object))                (assert-failure))
            ((not (eq? (lang-error-kind object) kind)) (assert-failure))
            (else (assert-success object)) ) )

) )

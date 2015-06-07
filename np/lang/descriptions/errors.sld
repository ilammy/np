(define-library (np lang descriptions errors)
  ;;;
  ;;; Error handling utilities
  ;;;
  (export lang-error
          lang-error?
          lang-error-kind
          lang-error-object
          lang-error-causes)

  (import (scheme base))

  (begin
    ;;;
    ;;; Error report object
    ;;;

    (define-record-type %lang-error
      (make-lang-error kind object causes)
      lang-error?
      (kind   lang-error-kind)
      (object lang-error-object)
      (causes lang-error-causes) )

    (define (lang-error kind object . causes)
      (make-lang-error kind object causes) )

) )

(define-library (np lang macros codegen-toplevel)
  ;;;
  ;;; Codegen for normalized toplevel clauses
  ;;;
  (export $generate-toplevel-predicate-definition
          $generate-toplevel-parser-definition
          $generate-toplevel-unparser-definition)

  (import (scheme base)
          (np lang descriptions language)
          (np lang descriptions types)
          (sr ck))

  (begin

    (define-syntax $generate-toplevel-predicate-definition
      (syntax-rules (quote)
        ((_ s 'lang '#f)
         ($ s '#t))
        ((_ s 'lang '(binding))
         ($ s '(define binding (language-predicate lang)))) ) )

    (define-syntax $generate-toplevel-parser-definition
      (syntax-rules (quote)
        ((_ s 'lang '#f)
         ($ s '#t))
        ((_ s 'lang '(binding))
         ($ s '(define binding (language-parser lang)))) ) )

    (define-syntax $generate-toplevel-unparser-definition
      (syntax-rules (quote)
        ((_ s 'lang '#f)
         ($ s '#t))
        ((_ s 'lang '(binding))
         ($ s '(define binding (language-unparser lang)))) ) )

) )

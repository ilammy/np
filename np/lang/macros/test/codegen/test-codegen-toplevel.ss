(import (scheme base)
        (np lang macros codegen)
        (np lang descriptions types)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:predicate "Code generation for toplevel predicate clauses")

  (define lang #f) ; expected to fail when define-language is implemented

  (define-syntax for
    (syntax-rules ()
      ((_ normalized-form . body)
       (begin
         ($ ($generate-toplevel-predicate-definition 'lang normalized-form))
         . body )) ) )

  (define (assert-defined procedure)
    (assert-true (procedure? procedure)) )

  ; This should be syntactically valid
  (define-test ("No predicate")
    (for '#f
      #t ) )

  (define-test ("Have predicate")
    (for '(lang?)
      (assert-defined lang?) ) )
)
(verify-test-case! toplevel:predicate)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:parser "Code generation for toplevel parser clauses")

  (define lang #f) ; expected to fail when define-language is implemented

  (define-syntax for
    (syntax-rules ()
      ((_ normalized-form . body)
       (begin
         ($ ($generate-toplevel-parser-definition 'lang normalized-form))
         . body )) ) )

  (define (assert-defined procedure)
    (assert-true (procedure? procedure)) )

  ; This should be syntactically valid
  (define-test ("No parser")
    (for '#f
      #t ) )

  (define-test ("Have parser")
    (for '(parse-lang)
      (assert-defined parse-lang) ) )
)
(verify-test-case! toplevel:parser)

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (toplevel:unparser "Code generation for toplevel unparser clauses")

  (define lang #f) ; expected to fail when define-language is implemented

  (define-syntax for
    (syntax-rules ()
      ((_ normalized-form . body)
       (begin
         ($ ($generate-toplevel-unparser-definition 'lang normalized-form))
         . body )) ) )

  (define (assert-defined procedure)
    (assert-true (procedure? procedure)) )

  ; This should be syntactically valid
  (define-test ("No unparser")
    (for '#f
      #t ) )

  (define-test ("Have unparser")
    (for '(unparse-lang)
      (assert-defined unparse-lang) ) )
)
(verify-test-case! toplevel:unparser)

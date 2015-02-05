(define-library (np lang descriptions language)
  ;;;
  ;;; Language description type and its associated accessors and procedures
  ;;;
  (export language-nonterminal-predicate
          language-predicate
          language-parser
          language-unparser)

  (import (scheme base))

  (begin

    (define (language-nonterminal-predicate language nonterminal-name)
      ;; Temporary definition
      (lambda (thing) #f) )

    (define (language-predicate language)
      ;; Temporary definition
      (lambda (thing) #f) )

    (define (language-parser language)
      ;; Temporary definition
      (lambda (thing) #f) )

    (define (language-unparser language)
      ;; Temporary definition
      (lambda (thing) #f) )

) )

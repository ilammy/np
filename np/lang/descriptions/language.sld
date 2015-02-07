(define-library (np lang descriptions language)
  ;;;
  ;;; Language description type and its associated accessors and procedures
  ;;;
  (export language-description language-description?
          make-standalone-language
          extend-existing-language

          language-nonterminal-predicate
          language-predicate
          language-parser
          language-unparser)

  (import (scheme base))

  (begin
    ;;;
    ;;; Language descriptions
    ;;;

    (define-record-type language-description
      ;; Temporary definition
      (make-language)
      language-description? )

    ;;;
    ;;; Language description constructors
    ;;;

    (define (make-standalone-language name terminal-definitions
              nonterminal-definitions )
      ;; Temporary definition
      (make-language) )

    (define (extend-existing-language extended-language name
              added-terminal-definitions removed-terminal-definitions
              modified-terminal-definitions added-nonterminal-definitions
              removed-nonterminal-definitions modified-nonterminal-definitions )
      ;; Temporary definition
      (make-language) )

    ;;;
    ;;; Language description accessors
    ;;;

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

(define-library (np lang impl macros structure-nonterminals)
  ;;;
  ;;; Structural analysis of nonterminal descriptions (standalone and extension)
  ;;;
  (export $can-be:standalone-nonterminal-description?
          $must-be:standalone-nonterminal-description)

  (import (scheme base)
          (sr ck)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates)
          (np lang impl macros structure-meta-vars)
          (np lang impl macros structure-productions)
          (np lang impl macros utils))

  (begin

    ;;;
    ;;; Nonterminal descriptions (standalone)
    ;;;

    ;; Check the form without predicate first to allow (Nonterm () ())
    (define-syntax $can-be:standalone-nonterminal-description?
      (syntax-rules (quote)
        ((_ s '(name (meta-vars ...) productions ...))
         ($ s ($and '($not-vector-or-list? 'name)
                    '($every? '$can-be:standalone-meta-var? '(meta-vars ...)) )))

        ((_ s '(name predicate (meta-vars ...) productions ...))
         ($ s ($and '($not-vector-or-list? 'name)
                    '($not-vector-or-list? 'predicate)
                    '($every? '$can-be:standalone-meta-var? '(meta-vars ...)) )))

        ((_ s _) ($ s '#f)) ) )

    (define-syntax $must-be:standalone-nonterminal-description
      (syntax-rules (quote)
        ((_ s 'lang '(name predicate (meta-vars ...) prod prods ...))
         ($ s ($append ($list 'name 'predicate
                         ($map '($must-be:standalone-meta-var 'lang 'name)
                               '(meta-vars ...) ) )
                       ($map '($must-be:standalone-production 'lang 'name)
                             '(prod prods ...) ) )))

        ((_ s 'lang '(name (meta-vars ...) prod prods ...))
         ($ s ($append ($list 'name
                         ($map '($must-be:standalone-meta-var 'lang 'name)
                               '(meta-vars ...) ) )
                       ($map '($must-be:standalone-production 'lang 'name)
                             '(prod prods ...) ) )))

        ((_ s 'lang '(name (meta-vars ...)))
         (syntax-error "Nonterminal must have at least one production" lang name))

        ((_ s 'lang '(name predicate (meta-vars ...)))
         (syntax-error "Nonterminal must have at least one production" lang name))

        ((_ s 'lang 'invalid-description)
         (syntax-error "Invalid terminal description syntax" lang invalid-description)) ) )

) )

(define-library (np lang impl macros structure-nonterminals)
  ;;;
  ;;; Structural analysis of nonterminal descriptions (standalone and extension)
  ;;;
  (export $can-be:standalone-nonterminal-description?
          $must-be:standalone-nonterminal-description
          
          $can-be:nonterminal-implicit-addition?
          $can-be:nonterminal-explicit-addition?
          $can-be:nonterminal-removal?
          $can-be:nonterminal-modification?
          $must-be:nonterminal-description-addition
          $must-be:nonterminal-description-removal
          $must-be:nonterminal-description-modification

          $get-nonterminal-modification-meta-vars
          $set-nonterminal-modification-meta-vars

          $get-nonterminal-modification-productions
          $set-nonterminal-modification-productions)

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
         (syntax-error "Invalid nonterminal description syntax" lang invalid-description)) ) )

    ;;;
    ;;; Nonterminal descriptions (extension, predicates)
    ;;;

    (define-syntax $can-be:nonterminal-explicit-addition?
      (syntax-rules (quote +)
        ((_ s '(+ clauses ...))
         ($ s ($every? '$can-be:standalone-nonterminal-description? '(clauses ...))))
        ((_ s _) ($ s '#f)) ) )

    (define-syntax $can-be:nonterminal-implicit-addition?
      (syntax-rules (quote)
        ((_ s 'expr) ($ s ($can-be:standalone-nonterminal-description? 'expr))) ) )

    
    (define-syntax $can-be:nonterminal-removal?
      (syntax-rules (quote -)
        ((_ s '(- clauses ...))
         ($ s ($every? '$standalone-description-or-name '(clauses ...))))
        ((_ s _) ($ s '#f)) ) )

    (define-syntax $standalone-description-or-name
      (syntax-rules (quote)
        ((_ s 'expr) ($ s ($or '($can-be:standalone-nonterminal-description? 'expr)
                               '($not-vector-or-list? 'expr) ))) ) )

    (define-syntax $can-be:nonterminal-modification?
      (syntax-rules (quote)
        ((_ s '(name predicate (vars ...) productions ...))
         ($ s ($every? '$can-be:extension-meta-var? '(vars ...))))

        ((_ s '(name (vars ...) productions ...))
         ($ s ($every? '$can-be:extension-meta-var? '(vars ...))))

        ((_ s _) ($ s '#f)) ) )
    
    ;;;
    ;;; Nonterminal descriptions (extension, assertions)
    ;;;

    (define-syntax $must-be:nonterminal-description-addition
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
         (syntax-error "Invalid nonterminal addition description syntax" lang invalid-description)) ) )

    (define-syntax $must-be:nonterminal-description-removal
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

        ((_ s 'lang '(some-other-list ...))
         (syntax-error "Invalid removed nonterminal removal description syntax" lang (some-other-list ...)))

        ((_ s 'lang '#(some-vector ...))
         (syntax-error "Invalid removed nonterminal removal description syntax" lang #(some-vector ...)))

        ((_ s _ 'otherwise-structurally-valid-nonterminal-name)
         ($ s 'otherwise-structurally-valid-nonterminal-name)) ) )

    (define-syntax $must-be:nonterminal-description-modification
      (syntax-rules (quote)
        ((_ s 'lang '(name predicate ((added-vars ...) (removed-vars ...))
                                     ((added-prods ...) (removed-prods ...)) ))
         ($ s ($list 'name 'predicate
                ($list ($map '($must-be:standalone-meta-var 'lang 'name)
                             '(added-vars ...) )
                       ($map '($must-be:standalone-meta-var 'lang 'name)
                             '(removed-vars ...) ) )
                ($list ($map '($must-be:standalone-production 'lang 'name)
                             '(added-prods ...) )
                       ($map '($must-be:standalone-production 'lang 'name)
                             '(removed-prods ...) ) ) )))

        ((_ s 'lang '(name ((added-vars ...) (removed-vars ...))
                           ((added-prods ...) (removed-prods ...)) ))
         ($ s ($list 'name
                ($list ($map '($must-be:standalone-meta-var 'lang 'name)
                             '(added-vars ...) )
                       ($map '($must-be:standalone-meta-var 'lang 'name)
                             '(removed-vars ...) ) )
                ($list ($map '($must-be:standalone-production 'lang 'name)
                             '(added-prods ...) )
                       ($map '($must-be:standalone-production 'lang 'name)
                             '(removed-prods ...) ) ) )))

        ((_ s 'lang 'invalid-description)
         (syntax-error "Invalid modified terminal description syntax" lang invalid-description)) ) )

    ;;;
    ;;; Getter/setter for subclauses in nonterminal modification descriptions
    ;;;

    (define-syntax $get-nonterminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s _ '(name predicate (vars ...) prods ...))
         ($ s '(vars ...)))
        ((_ s _ '(name (vars ...) prods ...))
         ($ s '(vars ...)))
        ((_ s 'lang 'expr)
         (syntax-error "Invalid nonterminal modification description" lang expr)) ) )

    (define-syntax $set-nonterminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s _ '(name predicate (vars ...) prods ...) '(new-vars ...))
         ($ s '(name predicate (new-vars ...) prods ...)))
        ((_ s _ '(name (vars ...) prods ...) '(new-vars ...))
         ($ s '(name (new-vars ...) prods ...)))
        ((_ s 'lang 'expr)
         (syntax-error "Invalid nonterminal modification description" lang expr)) ) )

    (define-syntax $get-nonterminal-modification-productions
      (syntax-rules (quote)
        ((_ s _ '(name predicate (vars ...) prods ...))
         ($ s '(prods ...)))
        ((_ s _ '(name (vars ...) prods ...))
         ($ s '(prods ...)))
        ((_ s 'lang 'expr)
         (syntax-error "Invalid nonterminal modification description" lang expr)) ) )

    (define-syntax $set-nonterminal-modification-productions
      (syntax-rules (quote)
        ((_ s _ '(name predicate (vars ...) prods ...) '(new-prods ...))
         ($ s '(name predicate (vars ...) (new-prods ...))))
        ((_ s _ '(name predicate (vars ...) prods ...) '(new-prods ...))
         ($ s '(name predicate (vars ...) (new-prods ...))))
        ((_ s 'lang 'expr)
         (syntax-error "Invalid nonterminal modification description" lang expr)) ) )

) )

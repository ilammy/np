(define-library (np lang impl macros structure-terminals)
  ;;;
  ;;; Structural analysis of terminal descriptions (standalone and extension)
  ;;;
  (export $can-be:standalone-terminal-description?
          $must-be:standalone-terminal-description

          $can-be:terminal-explicit-addition?
          $can-be:terminal-implicit-addition?
          $can-be:terminal-removal?
          $can-be:terminal-modification?
          $must-be:terminal-description-addition
          $must-be:terminal-description-removal
          $must-be:terminal-description-modification

          $get-terminal-modification-meta-vars
          $set-terminal-modification-meta-vars)

  (import (scheme base)
          (sr ck)
          (sr ck filters)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates)
          (np lang impl macros structure-meta-vars)
          (np lang impl macros utils))

  (begin

    ;;;
    ;;; Terminal descriptions (standalone)
    ;;;

    (define-syntax $can-be:standalone-terminal-description?
      (syntax-rules (quote)
        ((_ s '(name predicate (vars ...))) ($ s '#t))
        ((_ s '(predicate (vars ...)))      ($ s '#t))
        ((_ s _) ($ s '#f)) ) )

    (define-syntax $must-be:standalone-terminal-description
      (syntax-rules (quote)
        ((_ s 'lang '(name predicate ()))
         (syntax-error "Meta-var list cannot be empty" lang (name predicate ())))

        ((_ s 'lang '(predicate ()))
         (syntax-error "Meta-var list cannot be empty" lang (predicate ())))

        ((_ s 'lang '(() predicate (meta-vars ...)))
         (syntax-error "Terminal name must be a symbol" lang (() predicate (meta-vars ...)) ()))

        ((_ s 'lang '((x . xs) predicate (meta-vars ...)))
         (syntax-error "Terminal name must be a symbol" lang
                     ((x . xs) predicate (meta-vars ...)) (x . xs)))

        ((_ s 'lang '(#(x ...) predicate (meta-vars ...)))
         (syntax-error "Terminal name must be a symbol" lang
                     (#(x ...) predicate (meta-vars ...)) #(x ...)))

        ((_ s 'lang '((x . xs) (meta-vars ...)))
         (syntax-error "Predicate must be a symbol in short form" lang
                     ((x . xs) (meta-vars ...)) (x . xs)))

        ((_ s 'lang '(#(x ...) (meta-vars ...)))
         (syntax-error "Invalid terminal description syntax" lang
                     (#(x ...) (meta-vars ...))))

        ((_ s 'lang '(name predicate (meta-vars ...)))
         ($ s ($list 'name 'predicate
                ($map '($must-be:standalone-meta-var 'lang 'name)
                      '(meta-vars ...) ) )))

        ((_ s 'lang '(predicate (meta-vars ...)))
         ($ s ($list 'predicate
                ($map '($must-be:standalone-meta-var 'lang 'predicate)
                      '(meta-vars ...) ) )))

        ((_ s 'lang 'invalid-description)
         (syntax-error "Invalid terminal description syntax" lang invalid-description)) ) )

    ;;;
    ;;; Terminal descriptions (extension, predicates)
    ;;;

    (define-syntax $can-be:terminal-explicit-addition?
      (syntax-rules (quote +)
        ((_ s '(+ clauses ...))
         ($ s ($every? '$can-be:standalone-terminal-description? '(clauses ...))))
        ((_ s _) ($ s '#f)) ) )

    (define-syntax $can-be:terminal-implicit-addition?
      (syntax-rules (quote)
        ((_ s 'expr) ($ s ($can-be:standalone-terminal-description? 'expr))) ) )

    (define-syntax $can-be:terminal-removal?
      (syntax-rules (quote -)
        ((_ s '(- clauses ...))
         ($ s ($every? '$standalone-desciption-or-name? '(clauses ...))))
        ((_ s _) ($ s '#f)) ) )

    (define-syntax $standalone-desciption-or-name?
      (syntax-rules (quote)
        ((_ s 'expr) ($ s ($or '($can-be:standalone-terminal-description? 'expr)
                               '($not-vector-or-list? 'expr) ))) ) )

    (define-syntax $can-be:terminal-modification?
      (syntax-rules (quote)
        ((_ s '(name predicate (vars ...)))
         ($ s ($every? '$can-be:extension-meta-var? '(vars ...))))
        ((_ s '(name (vars ...)))
         ($ s ($every? '$can-be:extension-meta-var? '(vars ...))))
        ((_ s _) ($ s '#f)) ) )

    ;;;
    ;;; Terminal descriptions (extension, assertions)
    ;;;

    (define-syntax $must-be:terminal-description-addition
      (syntax-rules (quote)
        ((_ s 'lang '(name predicate ()))
         (syntax-error "Meta-var list cannot be empty" lang (name predicate ())))

        ((_ s 'lang '(predicate ()))
         (syntax-error "Meta-var list cannot be empty" lang (predicate ())))

        ((_ s 'lang '(name predicate (meta-vars ...)))
         ($ s ($list 'name 'predicate
                ($map '($must-be:standalone-meta-var 'lang 'name)
                      '(meta-vars ...) ) )))

        ((_ s 'lang '(predicate (meta-vars ...)))
         ($ s ($list 'predicate
                ($map '($must-be:standalone-meta-var 'lang 'predicate)
                      '(meta-vars ...) ) )))

        ((_ s 'lang 'invalid-description)
         (syntax-error "Invalid added terminal description syntax" lang invalid-description)) ) )

    (define-syntax $must-be:terminal-description-removal
      (syntax-rules (quote)
        ((_ s 'lang '(name predicate ()))
         (syntax-error "Meta-var list cannot be empty" lang (name predicate ())))

        ((_ s 'lang '(predicate ()))
         (syntax-error "Meta-var list cannot be empty" lang (predicate ())))

        ((_ s 'lang '(name predicate (meta-vars ...)))
         ($ s ($list 'name 'predicate
                ($map '($must-be:standalone-meta-var 'lang 'name)
                      '(meta-vars ...) ) )))

        ((_ s 'lang '(predicate (meta-vars ...)))
         ($ s ($list 'predicate
                ($map '($must-be:standalone-meta-var 'lang 'predicate)
                      '(meta-vars ...) ) )))

        ((_ s 'lang '(some-other-list ...))
         (syntax-error "Invalid removed terminal description syntax" lang (some-other-list ...)))

        ((_ s 'lang '#(some-vector ...))
         (syntax-error "Invalid removed terminal description syntax" lang #(some-vector ...)))

        ((_ s _ 'otherwise-structurally-valid-terminal-name)
         ($ s 'otherwise-structurally-valid-terminal-name)) ) )

    (define-syntax $must-be:terminal-description-modification
      (syntax-rules (quote)
        ((_ s 'lang '(name predicate ((added-vars ...) (removed-vars ...))))
         ($ s ($list 'name 'predicate
                ($list ($map '($must-be:standalone-meta-var 'lang 'name)
                             '(added-vars ...) )
                       ($map '($must-be:standalone-meta-var 'lang 'name)
                             '(removed-vars ...) ) ) )))

        ((_ s 'lang '(name ((added-vars ...) (removed-vars ...))))
         ($ s ($list 'name
                ($list ($map '($must-be:standalone-meta-var 'lang 'name)
                             '(added-vars ...) )
                       ($map '($must-be:standalone-meta-var 'lang 'name)
                             '(removed-vars ...) ) ) )))

        ((_ s 'lang 'invalid-description)
         (syntax-error "Invalid modified terminal description syntax" lang invalid-description)) ) )

    ;;;
    ;;; Getter/setter for meta-vars in terminal modification descriptions
    ;;;

    (define-syntax $get-terminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s 'lang '(name predicate meta-vars))
         ($ s 'meta-vars))

        ((_ s 'lang '(name meta-vars))
         ($ s 'meta-vars))

        ((_ s 'lang 'invalid-description)
         (syntax-error "Invalid modified terminal description syntax" lang invalid-description)) ) )

    (define-syntax $set-terminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s 'lang '(name predicate old-meta-vars) 'new-meta-vars)
         ($ s '(name predicate new-meta-vars)))

        ((_ s 'lang '(name old-meta-vars) 'new-meta-vars)
         ($ s '(name new-meta-vars)))

        ((_ s 'lang 'invalid-description)
         (syntax-error "Invalid modified terminal description syntax" lang invalid-description)) ) )

) )

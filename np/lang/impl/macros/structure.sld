(define-library (np lang impl macros structure)
  ;;;
  ;;; Macrofunctions that match and check the structure of clauses. They only
  ;;; check the structure of forms--count and positions of their elements, but
  ;;; not types of the elements or their semantics.
  ;;;
  ;;; ($can-be:<something>? <expr>) predicates check whether an <expr>
  ;;; _can be_ <something>. Getting #t as a result is necessary for the <expr>
  ;;; to be <something>. Getting #f means <expr> is definitely not <something>.
  ;;;
  ;;; ($must-be:<something> <expr>) assertions require that an <expr>
  ;;; _is_ <something>. Passing a $must-be assertion is sufficent for
  ;;; the <expr> to be considered <something>. These forms return the <expr>
  ;;; if the assertion holds, and fail with syntax error if it is not holding.
  ;;;
  (export $can-be:extends-clause?
          $can-be:predicate-clause?
          $can-be:parser-clause?
          $can-be:unparser-clause?
          $can-be:terminals-clause?
          $can-be:nonterminal-clause?

          $can-be:standalone-terminal-description?
          $must-be:standalone-terminal-description

          $can-be:terminal-explicit-addition?
          $can-be:terminal-implicit-addition?
          $can-be:terminal-removal?
          $can-be:terminal-modification?
          $must-be:terminal-description-addition
          $must-be:terminal-description-removal
          $must-be:terminal-description-modification)

  (import (scheme base)
          (sr ck)
          (sr ck lists)
          (sr ck maps))

  (begin
    ;;;
    ;;; Utilities
    ;;;

    (define-syntax $not-vector-or-list?
      (syntax-rules (quote)
        ((_ s '(things ...)) ($ s '#f))
        ((_ s '#(stuff ...)) ($ s '#f))
        ((_ s 'atom)         ($ s '#t)) ) )

    ;;;
    ;;; Toplevel clauses
    ;;;

    (define-syntax $can-be:extends-clause?
      (syntax-rules (quote extends)
        ((_ s '(extends   . _)) ($ s '#t))
        ((_ s        _        ) ($ s '#f)) ) )

    (define-syntax $can-be:predicate-clause?
      (syntax-rules (quote predicate)
        ((_ s '(predicate . _)) ($ s '#t))
        ((_ s        _        ) ($ s '#f)) ) )

    (define-syntax $can-be:parser-clause?
      (syntax-rules (quote parser)
        ((_ s '(parser    . _)) ($ s '#t))
        ((_ s        _        ) ($ s '#f)) ) )

    (define-syntax $can-be:unparser-clause?
      (syntax-rules (quote unparser)
        ((_ s '(unparser  . _)) ($ s '#t))
        ((_ s        _        ) ($ s '#f)) ) )

    (define-syntax $can-be:terminals-clause?
      (syntax-rules (quote terminals)
        ((_ s '(terminals . _)) ($ s '#t))
        ((_ s        _        ) ($ s '#f)) ) )

    (define-syntax $can-be:nonterminal-clause?
      (syntax-rules (quote)
        ((_ s '(something . _)) ($ s '#t))
        ((_ s        _        ) ($ s '#f)) ) )

    ;;;
    ;;; Terminal descriptions (standalone)
    ;;;

    (define-syntax $can-be:standalone-terminal-description?
      (syntax-rules (quote)
        ((_ s '(name predicate (vars ...)))
         ($ s ($every? '$can-be:standalone-meta-var? '(vars ...))))
        ((_ s '(predicate (vars ...)))
         ($ s ($every? '$can-be:standalone-meta-var? '(vars ...))))
        ((_ s _) ($ s '#f)) ) )

    (define-syntax $must-be:standalone-terminal-description
      (syntax-rules (quote)
        ((_ s 'lang '(name predicate ()))
         (syntax-error "Meta-var list cannot be empty"
                       lang (name predicate ())))

        ((_ s 'lang '(predicate ()))
         (syntax-error "Meta-var list cannot be empty"
                       lang (predicate ())))

        ((_ s 'lang '(name predicate (meta-vars ...)))
         ($ s ($list 'name 'predicate
                ($map '($must-be:standalone-meta-var 'lang 'name)
                      '(meta-vars ...) ) )))

        ((_ s 'lang '(predicate (meta-vars ...)))
         ($ s ($list 'predicate
                ($map '($must-be:standalone-meta-var 'lang 'predicate)
                      '(meta-vars ...) ) )))

        ((_ s 'lang 'invalid-description)
         (syntax-error "Invalid terminal description syntax"
                       lang invalid-description)) ) )

    ;;;
    ;;; Terminal descriptions (extension, predicates)
    ;;;

    (define-syntax $can-be:terminal-explicit-addition?
      (syntax-rules (quote +)
        ((_ s '(+ clauses ...))
         ($ s ($every? '$can-be:standalone-terminal-description?
                       '(clauses ...) )))
        ((_ s _) ($ s '#f)) ) )

    (define-syntax $can-be:terminal-implicit-addition?
      (syntax-rules (quote + -)
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
        ((_ s 'lang 'expr) ($ s 'expr)) ) )

    (define-syntax $must-be:terminal-description-removal
      (syntax-rules (quote)
        ((_ s 'lang 'expr) ($ s 'expr)) ) )

    (define-syntax $must-be:terminal-description-modification
      (syntax-rules (quote)
        ((_ s 'lang 'expr) ($ s 'expr)) ) )

    ;;;
    ;;; Meta-variables
    ;;;

    (define-syntax $can-be:standalone-meta-var?
      (syntax-rules (quote)
        ((_ s 'expr) ($ s ($not-vector-or-list? 'expr))) ) )

    (define-syntax $must-be:standalone-meta-var
      (syntax-rules (quote)
        ((_ s 'lang 'clause '(x ...))
         (syntax-error "Meta-var name cannot be a list" lang clause (x ...)))

        ((_ s 'lang 'clause '#(x ...))
         (syntax-error "Meta-var name cannot be a vector" lang clause #(x ...)))

        ((_ s _ _ 'var) ($ s 'var)) ) )

    (define-syntax $can-be:extension-meta-var?
      (syntax-rules (quote + -)
        ((_ s '(+ vars ...))
         ($ s ($every? '$can-be:standalone-meta-var? '(vars ...))))
        ((_ s '(- vars ...))
         ($ s ($every? '$can-be:standalone-meta-var? '(vars ...))))
        ((_ s _) ($ s '#f)) ) )

) )

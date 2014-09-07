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
          $must-be:standalone-terminal-description)

  (import (scheme base)
          (sr ck)
          (sr ck lists)
          (sr ck maps))

  (begin
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
    ;;; Terminal descriptions
    ;;;

    (define-syntax $can-be:standalone-terminal-description?
      (syntax-rules (quote)
        ((_ s '(name predicate (vars ...))) ($ s '#t))
        ((_ s '(predicate (vars ...)))      ($ s '#t))
        ((_ s 'anything-other-than-that)    ($ s '#f)) ) )

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
    ;;; Meta-variables
    ;;;

    (define-syntax $must-be:standalone-meta-var
      (syntax-rules (quote)
        ((_ s 'lang 'clause '(x ...))
         (syntax-error "Meta-var name cannot be a list" lang clause (x ...)))

        ((_ s 'lang 'clause '#(x ...))
         (syntax-error "Meta-var name cannot be a vector" lang clause #(x ...)))

        ((_ s _ _ 'var) ($ s 'var)) ) )

) )

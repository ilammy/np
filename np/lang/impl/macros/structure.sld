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
          $can-be:nonterminal-clause?)

  (import (scheme base)
          (sr ck))

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

) )

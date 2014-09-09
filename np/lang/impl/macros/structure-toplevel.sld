(define-library (np lang impl macros structure-toplevel)
  ;;;
  ;;; Structural analysis of toplevel define-language clauses
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

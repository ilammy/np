(define-library (np lang impl macros partitioning-toplevel)
  ;;;
  ;;; Partitioning toplevel define-language clauses
  ;;;
  (export $partition-toplevel-clauses)

  (import (scheme base)
          (sr ck)
          (sr ck filters)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates)
          (np lang impl macros structure-toplevel))

  (begin

    ;;;
    ;;; Partitioning process is a bit tricky here so I will explain it.
    ;;;

    ;; First we filter out absolutely correct toplevel clauses that have some
    ;; uniquely identifying traits--all these have prefixes like (extends ...)
    ;; Remaining clauses will be all nonterminal clauses, incorrect extends,
    ;; predicate, etc. clauses, and some absolutely invalid clauses like 42.
    (define-syntax $partition-toplevel-clauses
      (syntax-rules (quote)
        ((_ s 'lang 'clauses)
         ($ s ($partition-invalid-toplevel-clauses 'lang
                ($multi-partition '($is-an:extends-clause?
                                    $is-a:predicate-clause?
                                    $is-a:parser-clause?
                                    $is-an:unparser-clause?
                                    $is-a:terminals-clause?)
                  'clauses ) ))) ) )

    ;; Now we filter out incorrect toplevel clauses. Anything that matches its
    ;; can-be predicate is deemed to be incorrect, as such *correct* clauses
    ;; must have been filtered out already by $partition-toplevel-clauses.
    ;; The exception are nonterminals, these ought to be correct. And anything
    ;; that does not match a single can-be predicate is absolutely incorrect.
    (define-syntax $partition-invalid-toplevel-clauses
      (syntax-rules (quote)
        ((_ s 'lang '(e r p u t uncertain))
         ($ s ($postprocess-partitioned-toplevel-clauses 'lang
                '(e r p u t)
                ($multi-partition '($can-be:extends-clause?
                                    $can-be:predicate-clause?
                                    $can-be:parser-clause?
                                    $can-be:unparser-clause?
                                    $can-be:terminals-clause?
                                    $can-be:nonterminals-clause?)
                  'uncertain ) ))) ) )

    ;; If we have no incorrect clauses (only nonterminals) then proceed with
    ;; postprocessing. Else report the incorrect clauses.
    (define-syntax $postprocess-partitioned-toplevel-clauses
      (syntax-rules (quote)
        ((_ s 'lang '(e r p u t) '(() () () () () n ()))
         ($ s ($list
                ($get-extended-language  ($only-one 'lang 'e '"Only one 'extends' clause can be specified"))
                ($get-language-predicate ($only-one 'lang 'r '"Only one 'predicate' clause can be specified"))
                ($get-language-parser    ($only-one 'lang 'p '"Only one 'parser' clause can be specified"))
                ($get-language-unparser  ($only-one 'lang 'u '"Only one 'unparser' clause can be specified"))
                ($squash-terminals-clauses 't)
                'n )))

        ((_ s 'lang _ '(e r p u t _ i))
         ($ s ($report-invalid-toplevel-clauses 'lang '(e r p u t i)))) ) )

    (define-syntax $report-invalid-toplevel-clauses
      (syntax-rules (quote)
        ((_ s 'lang '(extends predicates parsers unparsers terminals invalid))
         ($ s ($and '($every? '($must-be:extends-clause   'lang) 'extends)
                    '($every? '($must-be:predicate-clause 'lang) 'predicates)
                    '($every? '($must-be:parser-clause    'lang) 'parsers)
                    '($every? '($must-be:unparser-clause  'lang) 'unparsers)
                    '($every? '($must-be:terminals-clause 'lang) 'terminals)
                    '($map '($expected-a:toplevel-clause  'lang) 'invalid) ))) ) )

    (define-syntax $only-one
      (syntax-rules (quote)
        ((_ s 'lang '()        'message) ($ s '#f))
        ((_ s 'lang '(x)       'message) ($ s 'x))
        ((_ s 'lang '(x y ...) 'message) (syntax-error message lang x y ...)) ) )

) )

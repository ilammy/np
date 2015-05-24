(define-library (np lang macros partitioning)
  ;;;
  ;;; Partitioning clauses
  ;;;
  (export $partition-toplevel-clauses

          $filter-standalone-terminal-definitions
          $partition-extension-terminal-definitions

          $filter-standalone-nonterminal-definitions
          $partition-extension-nonterminal-definitions)

  (import (scheme base)
          (sr ck)
          (sr ck filters)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates)
          (np lang macros structure-toplevel)
          (np lang macros structure-terminals)
          (np lang macros structure-nonterminals)
          (np lang macros structure-meta-vars)
          (np lang macros structure-productions)
          (np lang macros verify-utils))

  (begin

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;
    ;;; Toplevel clauses
    ;;;

    ;;
    ;; Partitioning process is a bit tricky here so I will explain it.
    ;;

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


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;
    ;;; Terminal definitions
    ;;;

    ;;;
    ;;; Standalone form
    ;;;

    (define-syntax $filter-standalone-terminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang 'definitions)
         ($ s ($check-for-invalid-terminal-definitions 'lang
                ($partition '$is-a:standalone-terminal?
                            'definitions ) ))) ) )

    (define-syntax $check-for-invalid-terminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang '(all-valid-definitions ())) ($ s 'all-valid-definitions))

        ((_ s 'lang '(_ (invalid-definitions ...)))
         ($ s ($map '($must-be:standalone-terminal 'lang)
                    '(invalid-definitions ...) ))) ) )

    ;;;
    ;;; Extension form
    ;;;

    (define-syntax $partition-extension-terminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang 'definitions)
         ($ s ($postprocess-partitioned-extension-terminal-definitions 'lang
                ($multi-partition '($is-a:terminal-addition?
                                    $is-a:terminal-removal?
                                    $is-a:terminal-modification?)
                  'definitions ) ))) ) )

    (define-syntax $postprocess-partitioned-extension-terminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang '(additions removals modifications ()))
         ($ s ($list
                ($squash-extension-terminal-clauses 'additions)
                ($squash-extension-terminal-clauses 'removals)
                ($map '($partition-terminal-modification-meta-vars 'lang)
                      ($squash-extension-terminal-clauses 'modifications) ) )))

        ((_ s 'lang '(_ _ _ (invalid-definitions ...)))
         ($ s ($report-invalid-extension-terminal-definitions 'lang
                ($multi-partition '($can-be:terminal-addition?
                                    $can-be:terminal-removal?
                                    $can-be:terminal-modification?)
                  '(invalid-definitions ...) ) ))) ) )

    (define-syntax $report-invalid-extension-terminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang '(additions removals modifications incomprehensible))
         ($ s ($and '($every? '($must-be:terminal-addition     'lang) 'additions)
                    '($every? '($must-be:terminal-removal      'lang) 'removals)
                    '($every? '($must-be:terminal-modification 'lang) 'modifications)
                    '($map '($expected-a:terminal-definition   'lang) 'incomprehensible) ))) ) )

    ;;;
    ;;; Partitioning of meta-vars of the modification extension form
    ;;;

    ;; Validity of meta-variables has been already verified by $is-a:terminal-modification?
    ;; so we can assume it and use $can-be? checks here (which are more simple).

    (define-syntax $partition-terminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s 'lang 'terminal-modification)
         ($ s ($set-terminal-modification-meta-vars 'lang 'terminal-modification
                ($squash-terminal-modification-meta-vars
                  ($multi-partition '($can-be:meta-var-addition? $can-be:meta-var-removal?)
                    ($get-terminal-modification-meta-vars 'lang 'terminal-modification) ) ) ))) ) )

    (define-syntax $squash-terminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s '(added removed ()))
         ($ s ($list ($squash-extension-meta-variables 'added)
                     ($squash-extension-meta-variables 'removed) ))) ) )


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;
    ;;; Nonterminal definitions
    ;;;

    ;;;
    ;;; Standalone form
    ;;;

    (define-syntax $filter-standalone-nonterminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang 'definitions)
         ($ s ($check-for-invalid-nonterminal-definitions 'lang
                ($partition '$is-a:standalone-nonterminal?
                            'definitions ) ))) ) )

    (define-syntax $check-for-invalid-nonterminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang '(all-valid-definitions ())) ($ s 'all-valid-definitions))

        ((_ s 'lang '(_ (invalid-definitions ...)))
         ($ s ($map '($must-be:standalone-nonterminal 'lang)
                    '(invalid-definitions ...) ))) ) )

    ;;;
    ;;; Extension form
    ;;;

    (define-syntax $partition-extension-nonterminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang 'definitions)
         ($ s ($postprocess-partitioned-extension-nonterminal-definitions 'lang
                ($multi-partition '($is-a:nonterminal-addition?
                                    $is-a:nonterminal-removal?
                                    $is-a:nonterminal-modification?)
                  'definitions ) ))) ) )

    (define-syntax $postprocess-partitioned-extension-nonterminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang '(additions removals modifications ()))
         ($ s ($list
                ($squash-extension-nonterminal-clauses 'additions)
                ($squash-extension-nonterminal-clauses 'removals)
                ($map '($partition-nonterminal-modification-productions 'lang)
                  ($map '($partition-nonterminal-modification-meta-vars 'lang)
                        ($squash-extension-nonterminal-clauses 'modifications) ) ) )))

        ((_ s 'lang '(_ _ _ (invalid-definitions ...)))
         ($ s ($report-invalid-extension-nonterminal-definitions 'lang
                ($multi-partition '($can-be:nonterminal-addition?
                                    $can-be:nonterminal-removal?
                                    $can-be:nonterminal-modification?)
                  '(invalid-definitions ...) ) ))) ) )

    (define-syntax $report-invalid-extension-nonterminal-definitions
      (syntax-rules (quote)
        ((_ s 'lang '(additions removals modifications incomprehensible))
         ($ s ($and '($every? '($must-be:nonterminal-addition     'lang) 'additions)
                    '($every? '($must-be:nonterminal-removal      'lang) 'removals)
                    '($every? '($must-be:nonterminal-modification 'lang) 'modifications)
                    '($map '($expected-a:nonterminal-definition   'lang) 'incomprehensible) ))) ) )

    ;;;
    ;;; Partitioning of meta-vars and productions of the modification extension form
    ;;;

    ;; Validity of meta-variables and productions has been already verified by
    ;; $is-a:nonterminal-modification? so we can assume it and use $can-be? checks
    ;; here (which are more simple).

    (define-syntax $partition-nonterminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s 'lang 'nonterminal-modification)
         ($ s ($set-nonterminal-modification-meta-vars 'lang 'nonterminal-modification
                ($squash-nonterminal-modification-meta-vars
                  ($multi-partition '($can-be:meta-var-addition? $can-be:meta-var-removal?)
                    ($get-nonterminal-modification-meta-vars 'lang 'nonterminal-modification) ) ) ))) ) )

    (define-syntax $squash-nonterminal-modification-meta-vars
      (syntax-rules (quote)
        ((_ s '(added removed ()))
         ($ s ($list ($squash-extension-meta-variables 'added)
                     ($squash-extension-meta-variables 'removed) ))) ) )

    (define-syntax $partition-nonterminal-modification-productions
      (syntax-rules (quote)
        ((_ s 'lang 'nonterminal-modification)
         ($ s ($set-nonterminal-modification-productions 'lang 'nonterminal-modification
                ($squash-nonterminal-modification-productions
                  ($multi-partition '($can-be:production-addition? $can-be:production-removal?)
                    ($get-nonterminal-modification-productions 'lang 'nonterminal-modification) ) ) ))) ) )

    (define-syntax $squash-nonterminal-modification-productions
      (syntax-rules (quote)
        ((_ s '(added removed ()))
         ($ s ($list ($squash-extension-productions 'added)
                     ($squash-extension-productions 'removed) ))) ) )

) )

(define-library (np lang impl macros structure-toplevel)
  ;;;
  ;;; Structural analysis of toplevel define-language clauses
  ;;;
  (export  $can-be:extends-clause?
            $is-an:extends-clause?
          $must-be:extends-clause

           $can-be:predicate-clause?
             $is-a:predicate-clause?
          $must-be:predicate-clause

           $can-be:parser-clause?
             $is-a:parser-clause?
          $must-be:parser-clause

           $can-be:unparser-clause?
            $is-an:unparser-clause?
          $must-be:unparser-clause

           $can-be:terminals-clause?
             $is-a:terminals-clause?
          $must-be:terminals-clause

          $can-be:nonterminals-clause?
 
          $expected-a:toplevel-clause

          $squash-terminals-clauses

          $get-extended-language
          $get-language-predicate
          $get-language-parser
          $get-language-unparser)

  (import (scheme base)
          (sr ck)
          (sr ck maps)
          (sr ck lists)
          (np lang impl macros verify-utils))

  (begin

    ;;;
    ;;; Toplevel singluar forms
    ;;;
    ;;; These are quite similar so I thought...
    ;;;
    ;;; 'Oh my goodness! Shut me down. Machines building machines. How perverse'
    ;;;                                                               -- C-3PO

    (define-syntax define-simple-checkers
      (syntax-rules ::: ()
        ((_ &keyword ($is-a? $must-be $can-be?)
            (:name-error-message :empty-error-message :unique-error-message :invalid-error-message))
         (begin
           (define-syntax $can-be?
             (syntax-rules (quote &keyword)
               ((_ s '(&keyword . _)) ($ s '#t))
               ((_ s  _)              ($ s '#f)) ) )

           (define-standard-checked-verifier ($is-a? $must-be)
             (syntax-rules (quote &keyword)
               ((_ s '(k t) 'term '(&keyword name))      ($ s (%verify-name '(k (term . t)) 'name)))
               ((_ s '(k t) 'term '(&keyword))           ($ k '(:empty-error-message (term . t))))
               ((_ s '(k t) 'term '(&keyword names ...)) ($ k '(:unique-error-message (term . t))))
               ((_ s '(k t) 'term  _)                    ($ k '(:invalid-error-message (term . t)))) ) )

           (define-verifier/atom %verify-name (:name-error-message)) )) ) )

    (define-simple-checkers extends
      ($is-an:extends-clause? $must-be:extends-clause $can-be:extends-clause?)
      ("Name of the language to be extended must be a symbol"
       "Name of the language to be extended cannot be empty"
       "Only one language can be extended"
       "Invalid syntax of the extension clause") )

    (define-simple-checkers predicate
      ($is-a:predicate-clause? $must-be:predicate-clause $can-be:predicate-clause?)
      ("Name of the language predicate must be a symbol"
       "Name of the language predicate cannot be empty"
       "Only one language predicate name can be specified"
       "Invalid syntax of the predicate clause") )

    (define-simple-checkers parser
      ($is-a:parser-clause? $must-be:parser-clause $can-be:parser-clause?)
      ("Name of the language parser must be a symbol"
       "Name of the language parser cannot be empty"
       "Only one language parser name can be specified"
       "Invalid syntax of the parser clause") )

    (define-simple-checkers unparser
      ($is-an:unparser-clause? $must-be:unparser-clause $can-be:unparser-clause?)
      ("Name of the language unparser must be a symbol"
       "Name of the language unparser cannot be empty"
       "Only one language unparser name can be specified"
       "Invalid syntax of the unparser clause") )

    ;;;
    ;;; Terminals clause
    ;;;

    (define-syntax $can-be:terminals-clause?
      (syntax-rules (quote terminals)
        ((_ s '(terminals . _)) ($ s '#t))
        ((_ s  _)               ($ s '#f)) ) )

    (define-standard-checked-verifier ($is-a:terminals-clause? $must-be:terminals-clause)
      (syntax-rules (quote terminals)
        ((_ s '(k t) 'term '(terminals . (x ...))) ($ s '#t))

        ((_ s '(k t) 'term '(terminals . #(x ...)))
         ($ k '("Expected a list of terminal definitions" (#(x ...) term . t))))

        ((_ s '(k t) 'term '(terminals . (x y ... . d)))
         ($ k '("Unexpected dotted list in language definition" (d term . t))))

        ((_ s '(k t) 'term '(terminals . d))
         ($ k '("Expected a list of terminal definitions" (d term . t))))

        ((_ s '(k t) 'term _)
         ($ k '("Invalid syntax of the terminals clause" (term . t)))) ) )

    ;;;
    ;;; Toplevel getters
    ;;;

    ;; Assuming that invalid arguments are not possible. Here we get either #f
    ;; which means that no respective clause is present, or the clause. Return
    ;; the value in a list to be able to discern between 'default #f' and the
    ;; one we would get from (extends #f), for example.

    (define-syntax define-simple-getter
      (syntax-rules ()
        ((_ ($getter) &keyword)
         (define-syntax $getter
           (syntax-rules (quote &keyword)
             ((_ s '#f)              ($ s '#f))
             ((_ s '(&keyword name)) ($ s '(name))) ) )) ) )

    (define-simple-getter ($get-extended-language)  extends)
    (define-simple-getter ($get-language-predicate) predicate)
    (define-simple-getter ($get-language-parser)    parser)
    (define-simple-getter ($get-language-unparser)  unparser)

    ;;;
    ;;; Squashers, checkers, other random stuff
    ;;;

    ;; Pretty loose definition, but this will do.
    (define-syntax $can-be:nonterminals-clause?
      (syntax-rules (quote)
        ((_ s '(a . d)) ($ s '#t))
        ((_ s  _)       ($ s '#f)) ) )

    (define-syntax $expected-a:toplevel-clause
      (syntax-rules (quote)
        ((_ s 'lang 'invalid-clause)
         (syntax-error "Invalid syntax of the toplevel clause" lang invalid-clause)) ) )

    ;; Assuming that the argument is actually a list of terminal clauses
    (define-syntax $squash-terminals-clauses
      (syntax-rules (quote)
        ((_ s 'clauses) ($ s ($concatenate ($map '$cdr 'clauses)))) ) )

) )

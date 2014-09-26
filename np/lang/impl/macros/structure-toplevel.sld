(define-library (np lang impl macros structure-toplevel)
  ;;;
  ;;; Structural analysis of toplevel define-language clauses
  ;;;
  (export $is-an:extends-clause?
         $can-be:extends-clause?
        $must-be:extends-clause

           $is-a:predicate-clause?
         $can-be:predicate-clause?
        $must-be:predicate-clause

           $is-a:parser-clause?
         $can-be:parser-clause?
        $must-be:parser-clause

          $is-an:unparser-clause?
         $can-be:unparser-clause?
        $must-be:unparser-clause

           $is-a:terminals-clause?
         $can-be:terminals-clause?
        $must-be:terminals-clause

        $get-extended-language
        $get-language-predicate
        $get-language-parser
        $get-language-unparser

        $can-be:nonterminals-clause?
 
        $squash-terminals-clauses)

  (import (scheme base)
          (sr ck)
          (sr ck maps)
          (sr ck lists)
          (np lang impl macros utils))

  (begin

    ;;;
    ;;; Toplevel singluar forms
    ;;;
    ;;; These are quite similar so I thought...
    ;;;
    ;;; 'Oh my goodness! Shut me down. Machines building machines. How perverse'
    ;;;                                                               -- C-3PO

    (define-syntax define-simple-matchers
      (syntax-rules ::: ()
        ((_ &keyword ($is-a? $must-be $can-be?)
            (:name-error-message :empty-error-message :unique-error-message :invalid-error-message))
         (begin
           (define-syntax $can-be?
             (syntax-rules (quote &keyword)
               ((_ s '(&keyword . _)) ($ s '#t))
               ((_ s  _)              ($ s '#f)) ) )

           (define-syntax $is-a?
             (syntax-rules (quote)
               ((_ s 'term)
                ($ s ($verify-result:as-boolean ($trampoline 'term)))) ) )

           (define-syntax $must-be
             (syntax-rules (quote)
               ((_ s 'lang 'term)
                ($ s ($verify-result:syntax-error ($trampoline 'lang 'term)))) ) )

           (define-syntax $trampoline
             (syntax-rules (quote)
               ((_ s 'term)       ($ s (%verify '(s ())     'term 'term)))
               ((_ s 'lang 'term) ($ s (%verify '(s (lang)) 'term 'term))) ) )

           ;; Need to duplicate terms there to be able to simultaneously decompose
           ;; and match them as well as to pass them further. One cannot just
           ;; write (&keyword name) in the _template_ when &keyword is a literal,
           ;; not a pattern variable, as that will result into _another_ &keyword,
           ;; not the one from the original expression.
           (define-syntax %verify
             (syntax-rules (quote &keyword)
               ((_ s '(k t) 'term '(&keyword name)) ($ s (%verify-name '(k (term . t)) 'name)))

               ((_ s '(k t) 'term '(&keyword))
                ($ k '(:empty-error-message (term . t))))

               ((_ s '(k t) 'term '(&keyword several names ...))
                ($ k '(:unique-error-message (term . t))))

               ((_ s '(k t) 'invalid-syntax _)
                ($ k '(:invalid-error-message (invalid-syntax . t)))) ) )

           (define-syntax %verify-name
             (syntax-rules (quote)
               ((_ s '(k t) '())       ($ k '(:name-error-message (()       . t))))
               ((_ s '(k t) '(a . d))  ($ k '(:name-error-message ((a . d)  . t))))
               ((_ s '(k t) '#(x ...)) ($ k '(:name-error-message (#(x ...) . t))))
               ((_ s '(k t)  _)        ($ s '#t)) ) ) )) ) )

    (define-simple-matchers extends
      ($is-an:extends-clause? $must-be:extends-clause $can-be:extends-clause?)
      ("Name of the language to be extended must be a symbol"
       "Name of the language to be extended cannot be empty"
       "Only one language can be extended"
       "Invalid syntax of the extension clause") )

    (define-simple-matchers predicate
      ($is-a:predicate-clause? $must-be:predicate-clause $can-be:predicate-clause?)
      ("Name of the language predicate must be a symbol"
       "Name of the language predicate cannot be empty"
       "Only one language predicate name can be specified"
       "Invalid syntax of the predicate clause") )

    (define-simple-matchers parser
      ($is-a:parser-clause? $must-be:parser-clause $can-be:parser-clause?)
      ("Name of the language parser must be a symbol"
       "Name of the language parser cannot be empty"
       "Only one language parser name can be specified"
       "Invalid syntax of the parser clause") )

    (define-simple-matchers unparser
      ($is-an:unparser-clause? $must-be:unparser-clause $can-be:unparser-clause?)
      ("Name of the language unparser must be a symbol"
       "Name of the language unparser cannot be empty"
       "Only one language unparser name can be specified"
       "Invalid syntax of the unparser clause") )

    ;;;
    ;;; Terminals clause
    ;;;

    ;; Hand-coding this due to specific checks

    (define-syntax $can-be:terminals-clause?
      (syntax-rules (quote terminals)
        ((_ s '(terminals . _)) ($ s '#t))
        ((_ s  _)               ($ s '#f)) ) )

    (define-syntax $is-a:terminals-clause?
      (syntax-rules (quote)
        ((_ s 'term)
         ($ s ($verify-result:as-boolean ($verify:terminals-clause 'term)))) ) )

    (define-syntax $must-be:terminals-clause
      (syntax-rules (quote)
        ((_ s 'lang 'term)
          ($ s ($verify-result:syntax-error ($verify:terminals-clause 'lang 'term)))) ) )

    (define-syntax $verify:terminals-clause
      (syntax-rules (quote)
        ((_ s 'term)       ($ s (%verify:terminals-clause '(s ())     'term 'term)))
        ((_ s 'lang 'term) ($ s (%verify:terminals-clause '(s (lang)) 'term 'term))) ) )

    (define-syntax %verify:terminals-clause
      (syntax-rules (quote terminals)
        ((_ s '(k t) '(terminals . (x ...)) _) ($ s '#t))

        ((_ s '(k t) 'term '(terminals . #(x ...)))
         ($ k '("Expected a list of terminal definitions" (#(x ...) term . t))))

        ((_ s '(k t) 'term '(terminals . (x y ... . z)))
         ($ k '("Unexpected dotted list in language definition" (z term . t))))

        ((_ s '(k t) 'term '(terminals . atom))
         ($ k '("Expected a list of terminal definitions" (atom term . t))))

        ((_ s '(k t) 'invalid-syntax _)
         ($ k '("Invalid syntax of the terminals clause" (invalid-syntax . t)))) ) )

    ;;;
    ;;; Toplevel getters
    ;;;

    ;; Assuming that invalid arguments are not possible. Here we get either #f
    ;; which means that no respective clause is present, or the clause. Return
    ;; the value in a list to be able to discern between 'default #f' and the
    ;; one we would get from (extends #f), for example.
    (define-syntax $get-extended-language  
      (syntax-rules (quote extends)
        ((_ s '#f)             ($ s '#f))
        ((_ s '(extends name)) ($ s '(name))) ) )

    (define-syntax $get-language-predicate 
      (syntax-rules (quote predicate)
        ((_ s '#f)               ($ s '#f))
        ((_ s '(predicate name)) ($ s '(name))) ) )

    (define-syntax $get-language-parser    
      (syntax-rules (quote parser)
        ((_ s '#f)            ($ s '#f))
        ((_ s '(parser name)) ($ s '(name))) ) )

    (define-syntax $get-language-unparser  
      (syntax-rules (quote unparser)
        ((_ s '#f)              ($ s '#f))
        ((_ s '(unparser name)) ($ s '(name))) ) )

    ;;;
    ;;; Random stuff
    ;;;

    ;; Pretty loose definition, but this will do.
    (define-syntax $can-be:nonterminals-clause?
      (syntax-rules (quote)
        ((_ s '(a . d)) ($ s '#t))
        ((_ s  _)       ($ s '#f)) ) )

    ;; Assuming that the argument is actually a list of terminal clauses
    (define-syntax $squash-terminals-clauses
      (syntax-rules (quote)
        ((_ s 'clauses) ($ s ($concatenate ($map '$cdr 'clauses)))) ) )

) )

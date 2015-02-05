(define-library (np lang macros define-language)
  ;;;
  ;;; The 'define-language' interface macro
  ;;;
  (export define-language)

  (import (scheme base)
          (sr ck)
          (sr ck lists)
          (sr ck maps)
          (sr ck predicates)
          (np lang descriptions language)
          (np lang macros partitioning-toplevel)
          (np lang macros partitioning-terminals)
          (np lang macros partitioning-nonterminals)
          (np lang macros normalization-terminals)
          (np lang macros normalization-nonterminals)
          (np lang macros codegen-toplevel)
          (np lang macros codegen-terminals)
          (np lang macros codegen-nonterminals)
          (np lang macros verify-utils))

  (begin
    ;;;
    ;;; Entry point
    ;;;

    (define-syntax define-language
      (syntax-rules ()
        ((_ . expressions) ($ ($define-language 'expressions))) ) )

    (define-syntax $define-language
      (syntax-rules (quote)
        ((_ s '(language . clauses))
         ($ s ($and '($must-be:language-name 'language)
                    '($must-be:toplevel-clauses-list 'language 'clauses)
                    '($define-language:partitioned 'language
                       ($partition-toplevel-clauses 'language 'clauses) ) )))
        ((_ s _)
         (syntax-error "Invalid syntax of the language definition")) ) )

    ;;;
    ;;; Normalization
    ;;;

    (define-syntax $define-language:partitioned
      (syntax-rules (quote)
        ((_ s 'language '(#false predicate parser unparser terminals nonterminals))
         ($ s ($define-language:standalone 'language 'predicate 'parser 'unparser
                ($normalized-standalone-terminals    'language 'terminals)
                ($normalized-standalone-nonterminals 'language 'nonterminals) )))

        ((_ s 'language '(extends predicate parser unparser terminals nonterminals))
         ($ s ($define-language:extension 'language 'extends 'predicate 'parser 'unparser
                ($normalized-extension-terminals    'language 'terminals)
                ($normalized-extension-nonterminals 'language 'nonterminals) ))) ) )

    (define-syntax $normalized-standalone-terminals
      (syntax-rules (quote)
        ((_ s 'language 'terminals)
         ($ s ($map '$normalize-standalone-terminal-definition
                ($filter-standalone-terminal-definitions 'language 'terminals) ))) ) )

    (define-syntax $normalized-standalone-nonterminals
      (syntax-rules (quote)
        ((_ s 'language 'nonterminals)
         ($ s ($map '$normalize-standalone-nonterminal-definition
                ($filter-standalone-nonterminal-definitions 'language 'nonterminals) ))) ) )

    (define-syntax $normalized-extension-terminals
      (syntax-rules (quote)
        ((_ s 'language 'terminals)
         ($ s ($normalize-extension-terminals
           ($partition-extension-terminal-definitions 'language 'terminals) ))) ) )

    (define-syntax $normalized-extension-nonterminals
      (syntax-rules (quote)
        ((_ s 'language 'nonterminals)
         ($ s ($normalize-extension-nonterminals
           ($partition-extension-nonterminal-definitions 'language 'nonterminals) ))) ) )

    (define-syntax $normalize-extension-terminals
      (syntax-rules (quote)
        ((_ s '(additions removals modifications))
         ($ s ($list ($map '$normalize-extension-terminal-addition     'additions)
                     ($map '$normalize-extension-terminal-removal      'removals)
                     ($map '$normalize-extension-terminal-modification 'modifications) ))) ) )

    (define-syntax $normalize-extension-nonterminals
      (syntax-rules (quote)
        ((_ s '(additions removals modifications))
         ($ s ($list ($map '$normalize-extension-nonterminal-addition     'additions)
                     ($map '$normalize-extension-nonterminal-removal      'removals)
                     ($map '$normalize-extension-nonterminal-modification 'modifications) ))) ) )

    ;;;
    ;;; Code generation
    ;;;

    (define-syntax $define-language:standalone
      (syntax-rules (quote)
        ((_ s 'language 'predicate 'parser 'unparser 'terminal-defintions 'nonterminal-defintions)
         ($ s ($generate-language-definitions:standalone      'language
                ($generate-toplevel-predicate-definition      'language 'predicate)
                ($generate-toplevel-parser-definition         'language 'parser)
                ($generate-toplevel-unparser-definition       'language 'unparser)
                ($generate-standalone-terminal-definitions    'terminal-defintions)
                ($generate-standalone-nonterminal-definitions 'nonterminal-defintions)
                ($generate-standalone-nonterminal-predicate-definitions
                  'language 'nonterminal-defintions ) ))) ) )

    (define-syntax $define-language:extension
      (syntax-rules (quote)
        ((_ s 'language '(extension) 'predicate 'parser 'unparser
              '(   terminal-additions    terminal-removals    terminal-modifications)
              '(nonterminal-additions nonterminal-removals nonterminal-modifications) )
         ($ s ($generate-language-definitions:extension        'language 'extension
                ($generate-toplevel-predicate-definition       'language 'predicate)
                ($generate-toplevel-parser-definition          'language 'parser)
                ($generate-toplevel-unparser-definition        'language 'unparser)
                ($generate-extension-terminal-additions        'terminal-additions)
                ($generate-extension-terminal-removals         'terminal-removals)
                ($generate-extension-terminal-modifications    'terminal-modifications)
                ($generate-extension-nonterminal-additions     'nonterminal-additions)
                ($generate-extension-nonterminal-removals      'nonterminal-removals)
                ($generate-extension-nonterminal-modifications 'nonterminal-modifications)
                ($generate-extension-nonterminal-predicate-definitions
                  'language 'nonterminal-additions ) ))) ) )

    (define-syntax $generate-language-definitions:standalone
      (syntax-rules (quote)
        ((_ s 'language 'define-predicate 'define-parser 'define-unparser
              'terminal-definitions 'nonterminal-definitions
              'define-nonterminal-predicates )
         ($ s '(begin
                 (define language
                   (make-standalone-language 'language
                     terminal-definitions nonterminal-definitions ) )
                 define-predicate
                 define-parser
                 define-unparser
                 define-nonterminal-predicates ))) ) )

    (define-syntax $generate-language-definitions:extension
      (syntax-rules (quote)
        ((_ s 'language 'extended-language
              'define-predicate
              'define-parser
              'define-unparser
              'terminal-definition-additions
              'terminal-definition-removals
              'terminal-definition-modifications
              'nonterminal-definition-additions
              'nonterminal-definition-removals
              'nonterminal-definition-modifications
              'define-nonterminal-predicates )
         ($ s '(begin
                 (define language
                   (extend-existing-language extended-language 'language
                     terminal-definition-additions
                     terminal-definition-removals
                     terminal-definition-modifications
                     nonterminal-definition-additions
                     nonterminal-definition-removals
                     nonterminal-definition-modifications ) )
                 define-predicate
                 define-parser
                 define-unparser
                 define-nonterminal-predicates ))) ) )

    ;;;
    ;;; Verifiers
    ;;;

    (define-standard-checkers %verify:toplevel-clauses-list
      ($is-a:toplevel-clauses-list? $must-be:toplevel-clauses-list) )

    (define-toplevel-checkers %verify:language-name
      ($is-a:language-name? $must-be:language-name) )

    (define-verifier/symbol %verify:language-name
      ("Name of the language must be an identifier") )

    (define-verifier/proper-list %verify:toplevel-clauses-list
      ("Unexpected dotted list in language definition"
       "Expected a list of toplevel clauses") )

) )

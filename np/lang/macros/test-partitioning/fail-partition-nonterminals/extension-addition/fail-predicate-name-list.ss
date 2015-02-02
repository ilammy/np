; Name of the nonterminal predicate must be an identifier
;   lang
;   (Nonterminal #((lambda (x) (Really-Nonterminal? x))) ())
;   (lambda (x) (Really-Nonterminal? x))
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (Nonterminal #((lambda (x) (Really-Nonterminal? x))) () production))) ) ))

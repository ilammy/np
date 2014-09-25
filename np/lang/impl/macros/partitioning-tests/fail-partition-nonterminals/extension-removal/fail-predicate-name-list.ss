; Name of the nonterminal predicate must be a symbol
;   lang
;   (Nonterminal #((lambda (x) (Really-Nonterminal? x))) ())
;   (lambda (x) (Really-Nonterminal? x))
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((- (Nonterminal #((lambda (x) (Really-Nonterminal? x))) () production))) ) ))

; Name of the meta-variable must be a symbol
;   lang
;   Number
;   (a . d)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((- (Number #(Pred?) ((a . d)) n1 n2))) ) ))

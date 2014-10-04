; Name of the meta-variable must be a symbol
;   lang
;   Number
;   (a . d)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-definitions 'lang
    '((Number #(Pred?) ((a . d)) n1 n2)) ) ))

; Name of the nonterminal must be a symbol
;   lang
;   (#(1) (some vars))
;   #(1)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-descriptions 'lang
    '((#(1) (some vars) production1)) ) ))

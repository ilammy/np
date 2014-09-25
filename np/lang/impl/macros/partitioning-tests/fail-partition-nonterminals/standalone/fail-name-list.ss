; Name of the nonterminal must be a symbol
;   lang
;   ((some list) #(pred?) (some vars))
;   (some list)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-descriptions 'lang
    '(((some list) #(pred?) (some vars) production1)) ) ))

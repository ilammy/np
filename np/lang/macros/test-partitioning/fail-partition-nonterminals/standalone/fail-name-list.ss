; Name of the nonterminal must be an identifier
;   lang
;   ((some list) #(pred?) (some vars))
;   (some list)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-definitions 'lang
    '(((some list) #(pred?) (some vars) production1)) ) ))

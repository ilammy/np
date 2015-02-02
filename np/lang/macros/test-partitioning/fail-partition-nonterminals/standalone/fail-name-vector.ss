; Name of the nonterminal must be an identifier
;   lang
;   (#(1) (some vars))
;   #(1)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-definitions 'lang
    '((#(1) (some vars) production1)) ) ))

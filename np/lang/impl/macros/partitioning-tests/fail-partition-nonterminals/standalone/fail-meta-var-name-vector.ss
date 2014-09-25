; Name of the meta-variable must be a symbol
;   lang
;   Number
;   #(9)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-descriptions 'lang
    '((Number (#(9)) n)) ) ))

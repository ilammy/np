; Invalid syntax of the nonterminal
;   lang
;   42
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-definitions 'lang
    '(42) ) ))

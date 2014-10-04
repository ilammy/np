; Invalid syntax of the nonterminal
;   lang
;   (Number #(pred))
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-definitions 'lang
    '((Number #(pred))) ) ))

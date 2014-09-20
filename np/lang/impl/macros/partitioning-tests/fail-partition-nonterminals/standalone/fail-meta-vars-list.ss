; Meta-variable name must be a symbol
;   lang
;   Number
;   (+ x)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-descriptions 'lang
    '((Number ((+ x)) n)) ) ))

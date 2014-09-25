; Name of the nonterminal must be a symbol
;   lang
;   (() (var))
;   ()
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-descriptions 'lang
    '((() (var) production1 (production2 production3))) ) ))

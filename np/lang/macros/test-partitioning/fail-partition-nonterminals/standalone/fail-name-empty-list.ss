; Name of the nonterminal must be an identifier
;   lang
;   (() (var))
;   ()
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-definitions 'lang
    '((() (var) production1 (production2 production3))) ) ))
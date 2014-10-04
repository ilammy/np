; Name of the nonterminal must be a symbol
;   lang
;   (() (var))
;   ()
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (() (var) production1 (production2 production3)))) ) ))

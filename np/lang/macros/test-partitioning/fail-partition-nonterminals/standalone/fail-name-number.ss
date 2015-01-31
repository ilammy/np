; Name of the nonterminal must be an identifier
;   lang
;   (13 (some vars))
;   13
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-definitions 'lang
    '((13 (some vars) production1)) ) ))

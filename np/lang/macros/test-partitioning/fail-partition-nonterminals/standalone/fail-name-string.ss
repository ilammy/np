; Name of the nonterminal must be an identifier
;   lang
;   ("the" (some vars))
;   "the"
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-definitions 'lang
    '(("the" (some vars) production1)) ) ))

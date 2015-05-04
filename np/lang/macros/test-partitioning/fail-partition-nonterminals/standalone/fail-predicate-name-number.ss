; Name of the nonterminal predicate must be an identifier
;   lang
;   (Nonterminal -4 (var1 var2))
;   -4
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-definitions 'lang
    '((Nonterminal -4 (var1 var2) (p p))) ) ))

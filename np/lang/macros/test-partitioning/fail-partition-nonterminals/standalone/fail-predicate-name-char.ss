; Name of the nonterminal predicate must be an identifier
;   lang
;   (Nonterminal #\K (var1 var2))
;   #\K
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-definitions 'lang
    '((Nonterminal #\K (var1 var2) (p p))) ) ))

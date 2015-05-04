; Name of the nonterminal predicate must be an identifier
;   lang
;   (Nonterminal #(x y z) (var1 var2))
;   #(x y z)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-definitions 'lang
    '((Nonterminal #(x y z) (var1 var2) (p p))) ) ))

; Name of the nonterminal predicate must be an identifier
;   lang
;   (Nonterminal #(12) (var1 var2))
;   12
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((- (Nonterminal #(12) (var1 var2) (p p)))) ) ))

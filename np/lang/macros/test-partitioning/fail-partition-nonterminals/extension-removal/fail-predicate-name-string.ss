; Name of the nonterminal predicate must be an identifier
;   lang
;   (Nonterminal #("There") (var1 var2))
;   "There"
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((- (Nonterminal #("There") (var1 var2) (p p)))) ) ))

; Name of the nonterminal predicate must be an identifier
;   lang
;   (Nonterminal 6 (var1 var2))
;   6
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (Nonterminal 6 (var1 var2) (p p)))) ) ))

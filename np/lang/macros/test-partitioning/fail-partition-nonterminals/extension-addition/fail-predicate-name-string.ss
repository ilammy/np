; Name of the nonterminal predicate must be an identifier
;   lang
;   (Nonterminal "the" (var1 var2))
;   "the"
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (Nonterminal "the" (var1 var2) (p p)))) ) ))

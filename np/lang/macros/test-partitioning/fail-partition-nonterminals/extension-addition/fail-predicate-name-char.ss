; Name of the nonterminal predicate must be an identifier
;   lang
;   (Nonterminal #(#\c) (var1 var2))
;   #\c
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (Nonterminal #(#\c) (var1 var2) (p p)))) ) ))

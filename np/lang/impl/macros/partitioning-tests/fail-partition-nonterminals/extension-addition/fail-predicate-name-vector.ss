; Name of the nonterminal predicate must be a symbol
;   lang
;   (Nonterminal #(#(x y z)) (var1 var2))
;   #(x y z)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((+ (Nonterminal #(#(x y z)) (var1 var2) (p p)))) ) ))

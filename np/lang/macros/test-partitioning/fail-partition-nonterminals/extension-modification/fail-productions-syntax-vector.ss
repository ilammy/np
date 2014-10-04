; Expected a list of production modifications
;   lang
;   Pair
;   #(1 2 3)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Pair ((+ x)) . #(1 2 3)))) ) ))

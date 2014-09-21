; Invalid extension production syntax
;   lang
;   Pair
;   #(9)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((Pair ((+ x)) #(9))) ) ))

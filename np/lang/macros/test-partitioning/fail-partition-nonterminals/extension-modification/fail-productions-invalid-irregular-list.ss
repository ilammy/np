; Invalid syntax of the production modification
;   lang
;   Pair
;   (a . d)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Pair ((- x)) (a . d)))) ) ))

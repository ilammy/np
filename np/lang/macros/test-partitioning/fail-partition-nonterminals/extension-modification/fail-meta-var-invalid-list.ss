; Invalid syntax of the meta-variable modification
;   lang
;   Pair
;   (random list)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Pair ((- x) (+ y) (random list))))) ) ))

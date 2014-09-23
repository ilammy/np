; Invalid extension meta-variable syntax
;   lang
;   Pair
;   (random list)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((! (Pair ((- x) (+ y) (random list))))) ) ))

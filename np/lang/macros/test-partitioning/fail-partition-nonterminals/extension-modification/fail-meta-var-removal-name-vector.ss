; Name of the meta-variable must be a symbol
;   lang
;   Num
;   #(x)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Num ((- #(x)))))) ) ))

; Invalid syntax of the nonterminal extension
;   lang
;   #(3 14 15)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '(#(3 14 15)) ) ))

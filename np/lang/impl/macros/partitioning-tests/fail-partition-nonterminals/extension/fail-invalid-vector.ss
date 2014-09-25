; Invalid syntax of the nonterminal extension
;   lang
;   #(3 14 15)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '(#(3 14 15)) ) ))

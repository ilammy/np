; Invalid syntax of the nonterminal
;   lang
;   #(Number (num) n)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((+ #(Number (num) n))) ) ))

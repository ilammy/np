; Invalid syntax of the nonterminal
;   lang
;   #(Number (num) n)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((- #(Number (num) n))) ) ))
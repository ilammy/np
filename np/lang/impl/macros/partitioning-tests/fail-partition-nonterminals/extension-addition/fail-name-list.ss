; Name of the nonterminal must be a symbol
;   lang
;   ((some list) #(pred?) (some vars))
;   (some list)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ ((some list) #(pred?) (some vars) production1))) ) ))

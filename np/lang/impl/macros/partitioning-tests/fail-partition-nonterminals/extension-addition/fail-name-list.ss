; Nonterminal name must be a symbol
;   lang
;   ((some list) #(pred?) (some vars))
;   (some list)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((+ ((some list) #(pred?) (some vars) production1))) ) ))

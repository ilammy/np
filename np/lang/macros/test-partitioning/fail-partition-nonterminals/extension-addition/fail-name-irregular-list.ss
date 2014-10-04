; Name of the nonterminal must be a symbol
;   lang
;   ((car . cdr) #(pred?) ())
;   (car . cdr)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ ((car . cdr) #(pred?) () production1))) ) ))

; Invalid syntax of the nonterminal modification
;   lang
;   #(Number ((+ x)) n)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! #(Number ((+ x)) n))) ) ))

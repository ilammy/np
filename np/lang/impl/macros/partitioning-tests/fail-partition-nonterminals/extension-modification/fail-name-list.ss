; Name of the nonterminal must be a symbol
;   lang
;   ((awesome name) ())
;   (awesome name)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! ((awesome name) () (+ x)))) ) ))

; Meta-variable name must be a symbol
;   lang
;   Number
;   (x y)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((Number ((x y)) n)) ) ))

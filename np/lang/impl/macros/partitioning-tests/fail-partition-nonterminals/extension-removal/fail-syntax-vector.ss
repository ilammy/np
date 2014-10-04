; Expected a list of nonterminal definitions or names
;   lang
;   (- . #(Number #(pp) (n)))
;   #(Number #(pp) (n))
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((- . #(Number #(pp) (n)))) ) ))

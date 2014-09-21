; Nonterminal name must be a symbol
;   lang
;   (#(1) (some vars))
;   #(1)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((- (#(1) (some vars)))) ) ))

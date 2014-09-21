; Unexpected dotted list in nonterminal description
;   lang
;   Number
;   (prod1 . prod2)
;   prod2
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-descriptions 'lang
    '((Number () prod1 . prod2)) ) ))

; Expected production list
;   lang
;   Nonterminal
;   sudden-atom
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-descriptions 'lang
    '((Nonterminal #(p) () . sudden-atom)) ) ))

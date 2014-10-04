; Expected a list of productions
;   lang
;   Nonterminal
;   sudden-atom
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-definitions 'lang
    '((Nonterminal #(p) () . sudden-atom)) ) ))

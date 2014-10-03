; Unexpected dotted list in nonterminal definition
;   lang
;   Number
;   (a . d)
;   d
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-descriptions 'lang
    '((Number (a . d) n)) ) ))

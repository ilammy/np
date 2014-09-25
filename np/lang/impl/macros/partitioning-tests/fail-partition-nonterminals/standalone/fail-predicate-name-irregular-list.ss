; Name of the nonterminal predicate must be a symbol
;   lang
;   (Nonterminal #((a . d)) ())
;   (a . d)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-descriptions 'lang
    '((Nonterminal #((a . d)) () "technically incorrect production")) ) ))

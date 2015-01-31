; Name of the nonterminal predicate must be an identifier
;   lang
;   (Nonterminal #(()) (var))
;   ()
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-definitions 'lang
    '((Nonterminal #(()) (var) prod)) ) ))

; Only one nonterminal predicate name can be specified
;   lang
;   (Nonterminal #(a b c) ())
;   #(a b c)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-definitions 'lang
    '((Nonterminal #(a b c) () "technically incorrect production")) ) ))

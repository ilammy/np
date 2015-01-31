; Name of the meta-variable must be an identifier
;   lang
;   Number
;   #t
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-definitions 'lang
    '((Number (#t) n)) ) ))

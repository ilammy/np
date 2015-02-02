; Name of the meta-variable must be an identifier
;   lang
;   Number
;   #\i
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-definitions 'lang
    '((Number (#\i) n)) ) ))

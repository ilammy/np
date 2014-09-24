; Expected meta-variable list
;   lang
;   Number
;   SURPRISE!
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-descriptions 'lang
    '((Number SURPRISE! n)) ) ))

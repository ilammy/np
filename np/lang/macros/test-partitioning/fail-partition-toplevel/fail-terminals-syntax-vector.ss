; Expected a list of terminal definitions
;   lang
;   (terminals . #(some vector))
;   #(some vector)
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((terminals . #(some vector))) ) ))

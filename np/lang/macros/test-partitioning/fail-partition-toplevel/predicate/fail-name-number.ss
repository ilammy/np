; Name of the language predicate must be an identifier
;   lang
;   (predicate 2)
;   2
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((predicate 2)) ) ))

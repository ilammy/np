; Name of the language predicate must be a symbol
;   lang
;   (predicate (1 2 3))
;   (1 2 3)
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((predicate (1 2 3))) ) ))

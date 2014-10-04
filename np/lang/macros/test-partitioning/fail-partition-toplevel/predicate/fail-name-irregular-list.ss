; Name of the language predicate must be a symbol
;   lang
;   (predicate (a . d))
;   (a . d)
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((predicate (a . d))) ) ))

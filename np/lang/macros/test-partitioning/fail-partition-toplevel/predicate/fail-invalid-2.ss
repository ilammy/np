; Invalid syntax of the predicate clause
;   lang
;   (predicate . #())
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((predicate . #())) ) ))
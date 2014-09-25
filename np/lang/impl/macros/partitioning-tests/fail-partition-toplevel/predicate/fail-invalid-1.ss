; Invalid syntax of the predicate clause
;   lang
;   (predicate . 123)
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((predicate . 123)) ) ))

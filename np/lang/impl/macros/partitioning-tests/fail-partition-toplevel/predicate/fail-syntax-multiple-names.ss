; Only one language predicate name can be specified
;   lang
;   (predicate 1 2 3)
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((predicate 1 2 3)) ) ))

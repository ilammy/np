; Only one 'predicate' clause can be specified
;   lang
;   (predicate lang?_1)
;   (predicate lang?_2)
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((predicate lang?_1) (predicate lang?_2)) ) ))

; Name of the language predicate cannot be empty
;   lang
;   (predicate)
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((predicate)) ) ))

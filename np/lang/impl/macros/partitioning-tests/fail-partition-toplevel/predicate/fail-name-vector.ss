; Name of the language predicate must be a symbol
;   lang
;   (predicate #(some-pred))
;   #(some-pred)
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((predicate #(some-pred))) ) ))

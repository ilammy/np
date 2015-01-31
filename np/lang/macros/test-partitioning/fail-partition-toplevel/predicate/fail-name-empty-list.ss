; Name of the language predicate must be an identifier
;   lang
;   (predicate ())
;   ()
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((predicate ())) ) ))

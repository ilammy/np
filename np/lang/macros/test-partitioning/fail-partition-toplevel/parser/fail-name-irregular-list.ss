; Name of the language parser must be a symbol
;   lang
;   (parser (a . d))
;   (a . d)
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((parser (a . d))) ) ))

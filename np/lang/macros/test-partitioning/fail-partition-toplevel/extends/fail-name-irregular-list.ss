; Name of the language to be extended must be a symbol
;   lang
;   (extends (a . d))
;   (a . d)
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((extends (a . d))) ) ))

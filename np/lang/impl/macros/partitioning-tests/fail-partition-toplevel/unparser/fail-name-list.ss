; Name of the language unparser must be a symbol
;   lang
;   (unparser (1 2 3))
;   (1 2 3)
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((unparser (1 2 3))) ) ))

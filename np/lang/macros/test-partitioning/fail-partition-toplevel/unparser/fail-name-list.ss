; Name of the language unparser must be an identifier
;   lang
;   (unparser (1 2 3))
;   (1 2 3)
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((unparser (1 2 3))) ) ))

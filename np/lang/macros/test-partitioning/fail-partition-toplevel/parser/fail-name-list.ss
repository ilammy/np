; Name of the language parser must be an identifier
;   lang
;   (parser (1 2 3))
;   (1 2 3)
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((parser (1 2 3))) ) ))

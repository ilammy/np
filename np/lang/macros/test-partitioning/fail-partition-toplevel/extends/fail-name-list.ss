; Name of the language to be extended must be a symbol
;   lang
;   (extends (1 2 3))
;   (1 2 3)
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((extends (1 2 3))) ) ))

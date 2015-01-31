; Name of the language to be extended must be an identifier
;   lang
;   (extends #f)
;   #f
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((extends #f)) ) ))

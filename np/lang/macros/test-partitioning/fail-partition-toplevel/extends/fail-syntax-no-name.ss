; Name of the language to be extended cannot be empty
;   lang
;   (extends)
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((extends)) ) ))

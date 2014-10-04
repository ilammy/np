; Only one language can be extended
;   lang
;   (extends 1 2 3)
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((extends 1 2 3)) ) ))

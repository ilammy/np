; Invalid syntax of the unparser clause
;   lang
;   (unparser some dotted . list)
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((unparser some dotted . list)) ) ))

; Invalid syntax of the parser clause
;   lang
;   (parser some dotted . list)
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((parser some dotted . list)) ) ))

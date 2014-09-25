; Invalid syntax of the toplevel clause
;   lang
;   #(obviosly incorrect syntax)
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '(#(obviosly incorrect syntax)) ) ))

; Invalid syntax of the parser clause
;   lang
;   (parser . #())
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((parser . #())) ) ))

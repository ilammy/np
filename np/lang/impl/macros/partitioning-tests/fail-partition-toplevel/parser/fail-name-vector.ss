; Name of the language parser must be a symbol
;   lang
;   (parser #(some-parser))
;   #(some-parser)
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((parser #(some-parser))) ) ))

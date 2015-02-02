; Name of the language parser must be an identifier
;   lang
;   (parser #(some-parser))
;   #(some-parser)
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((parser #(some-parser))) ) ))

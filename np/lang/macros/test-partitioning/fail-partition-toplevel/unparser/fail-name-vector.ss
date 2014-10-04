; Name of the language unparser must be a symbol
;   lang
;   (unparser #(some-name))
;   #(some-name)
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((unparser #(some-name))) ) ))

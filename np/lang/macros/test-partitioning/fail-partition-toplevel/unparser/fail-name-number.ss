; Name of the language unparser must be an identifier
;   lang
;   (unparser 9)
;   9
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((unparser 9)) ) ))

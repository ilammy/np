; Only one language parser name can be specified
;   lang
;   (parser 1 2 3)
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((parser 1 2 3)) ) ))
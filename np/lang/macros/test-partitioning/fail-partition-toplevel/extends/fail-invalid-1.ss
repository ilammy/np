; Invalid syntax of the extension clause
;   lang
;   (extends . 123)
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((extends . 123)) ) ))

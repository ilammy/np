; Only one 'parser' clause allowed
; lang
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((parser parse-lang) (parser parse-lang) (parser parse-lang)) ) ))

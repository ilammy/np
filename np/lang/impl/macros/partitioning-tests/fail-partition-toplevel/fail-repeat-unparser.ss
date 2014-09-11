; Only one 'unparser' clause allowed
; lang
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((unparser unparse-lang) (parser parse-lang) (unparser unparse-lang)) ) ))

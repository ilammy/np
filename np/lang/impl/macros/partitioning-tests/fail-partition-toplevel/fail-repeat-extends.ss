; Only one 'extends' clause allowed
; lang
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((extends some-lang) (extends some-other-lang)) ) ))

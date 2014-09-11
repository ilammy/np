; Unexpected dotted list in language description
;   lang
;   some-atom
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((extends some-lang) . some-atom) ) ))

; Only one 'extends' clause can be specified
;   lang
;   (extends some-lang)
;   (extends some-other-lang)
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((extends some-lang) (extends some-other-lang)) ) ))

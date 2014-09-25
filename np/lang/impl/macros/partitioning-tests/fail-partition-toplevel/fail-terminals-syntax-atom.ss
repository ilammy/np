; Expected a list of terminal definitions
;   lang
;   (terminals . some-atom)
;   some-atom
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((terminals . some-atom)) ) ))

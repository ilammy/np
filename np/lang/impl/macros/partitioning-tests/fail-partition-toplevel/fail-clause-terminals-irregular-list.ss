; Unexpected dotted list in terminal description
;   lang
;   (terminals . some-atom)
;   some-atom
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((terminals (num number? (n))) (terminals . some-atom)) ) ))

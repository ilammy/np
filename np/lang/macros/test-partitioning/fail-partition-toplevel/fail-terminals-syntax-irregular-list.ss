; Invalid syntax of the terminals clause
;   lang
;   (terminals (num number? (n)) . some-atom)
(import (scheme base)
        (np lang macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((terminals (num number? (n)) . some-atom)) ) ))

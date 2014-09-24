; Unexpected dotted list in terminal description
;   lang
;   number
;   (a b . c)
;   c
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-descriptions 'lang
    '((number number? (a b . c))) ) ))

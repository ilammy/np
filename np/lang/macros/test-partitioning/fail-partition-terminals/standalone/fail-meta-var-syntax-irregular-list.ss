; Unexpected dotted list in terminal definition
;   lang
;   number
;   (a b . c)
;   c
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-definitions 'lang
    '((number number? (a b . c))) ) ))

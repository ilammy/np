; Name of the meta-variable must be an identifier
;   lang
;   Num
;   #\x1234
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-definitions 'lang
    '((Num number? (#\x1234))) ) ))

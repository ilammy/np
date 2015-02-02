; Name of the meta-variable must be an identifier
;   lang
;   Num
;   -6
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-definitions 'lang
    '((Num number? (-6))) ) ))

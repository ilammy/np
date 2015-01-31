; Name of the terminal must be an identifier
;   lang
;   (#\c predicate? (some vars))
;   #\c
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-definitions 'lang
    '((#\c predicate? (some vars))) ) ))

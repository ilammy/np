; Name of the terminal must be an identifier
;   lang
;   (#t predicate? (some vars))
;   #t
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-definitions 'lang
    '((#t predicate? (some vars))) ) ))

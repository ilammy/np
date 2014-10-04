; Name of the terminal must be a symbol
;   lang
;   ((car . cdr) predicate? (some vars))
;   (car . cdr)
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-definitions 'lang
    '(((car . cdr) predicate? (some vars))) ) ))

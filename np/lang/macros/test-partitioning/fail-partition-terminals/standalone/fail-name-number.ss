; Name of the terminal must be an identifier
;   lang
;   (1.0 predicate? (some vars))
;   1.0
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-definitions 'lang
    '((1.0 predicate? (some vars))) ) ))

; Name of the terminal must be a symbol
;   lang
;   (#() predicate? (some vars))
;   #()
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-definitions 'lang
    '((#() predicate? (some vars))) ) ))

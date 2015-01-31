; Name of the terminal must be an identifier
;   lang
;   ("at" predicate? (some vars))
;   "at"
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-definitions 'lang
    '(("at" predicate? (some vars))) ) ))

; Name of the terminal must be an identifier
;   lang
;   (() p? (some vars))
;   ()
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-definitions 'lang
    '((() p? (some vars))) ) ))

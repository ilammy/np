; Name of the terminal must be a symbol
;   lang
;   (() p? (some vars))
;   ()
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-definitions 'lang
    '((() p? (some vars))) ) ))

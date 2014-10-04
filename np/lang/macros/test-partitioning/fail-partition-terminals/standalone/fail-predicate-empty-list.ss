; Terminal predicate must be a variable in short form
;   lang
;   (() (some vars))
;   ()
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-definitions 'lang
    '((() (some vars))) ) ))

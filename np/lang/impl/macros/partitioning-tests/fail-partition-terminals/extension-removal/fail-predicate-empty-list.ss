; Terminal predicate must be a variable in short form
;   lang
;   (() (some vars))
;   ()
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((- (() (some vars)))) ) ))

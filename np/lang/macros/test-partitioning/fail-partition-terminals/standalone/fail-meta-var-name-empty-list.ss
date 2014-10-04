; Name of the meta-variable must be a symbol
;   lang
;   number?
;   ()
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-definitions 'lang
    '((number? (()))) ) ))

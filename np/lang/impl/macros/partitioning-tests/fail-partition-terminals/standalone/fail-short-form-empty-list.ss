; Predicate must be a symbol in short form
;   lang
;   (() (some vars))
;   ()
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-descriptions 'lang
    '((() (some vars))) ) ))

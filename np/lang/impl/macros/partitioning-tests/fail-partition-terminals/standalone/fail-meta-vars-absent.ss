; Terminal must have at least one meta-variable
;   lang
;   (number? ())
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-descriptions 'lang
    '((number? ())) ) ))

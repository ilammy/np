; At least one meta-variable should be specified for a terminal
;   lang
;   number?
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-descriptions 'lang
    '((number? ())) ) ))

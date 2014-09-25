; Expected a list of meta-variables
;   lang
;   number?
;   foo
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-descriptions 'lang
    '((number? foo)) ) ))

; Meta-variable name must be a symbol
;   lang
;   number?
;   (+ x)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-descriptions 'lang
    '((number? ((+ x)))) ) ))

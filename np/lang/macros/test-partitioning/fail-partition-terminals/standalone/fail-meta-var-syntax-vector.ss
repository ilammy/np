; Invalid syntax of the terminal
;   lang
;   (number? #(1 2 3))
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-definitions 'lang
    '((number? #(1 2 3))) ) ))

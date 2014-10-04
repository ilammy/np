; Invalid syntax of the terminal
;   lang
;   (a b . c)
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-definitions 'lang
    '((a b . c)) ) ))

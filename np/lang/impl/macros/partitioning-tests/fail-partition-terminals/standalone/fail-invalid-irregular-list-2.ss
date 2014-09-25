; Invalid syntax of the terminal
;   lang
;   (a b . c)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-descriptions 'lang
    '((a b . c)) ) ))

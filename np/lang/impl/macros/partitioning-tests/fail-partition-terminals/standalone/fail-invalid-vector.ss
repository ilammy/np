; Invalid syntax of the terminal
;   lang
;   #(symbol? (s))
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-definitions 'lang
    '(#(symbol? (s))) ) ))

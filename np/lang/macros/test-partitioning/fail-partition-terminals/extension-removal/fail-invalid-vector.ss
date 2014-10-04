; Invalid syntax of the terminal
;   lang
;   #(symbol? (s))
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((- #(symbol? (s)))) ) ))

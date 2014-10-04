; Invalid syntax of the terminal extension
;   lang
;   #(3 14 15)
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '(#(3 14 15)) ) ))

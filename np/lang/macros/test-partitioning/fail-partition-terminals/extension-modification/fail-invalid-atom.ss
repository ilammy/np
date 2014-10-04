; Invalid syntax of the terminal modification
;   lang
;   a-symbol
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! a-symbol)) ) ))

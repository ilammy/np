; Invalid syntax of the terminal modification
;   lang
;   (num ((- x) . foo))
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! (num ((- x) . foo)))) ) ))

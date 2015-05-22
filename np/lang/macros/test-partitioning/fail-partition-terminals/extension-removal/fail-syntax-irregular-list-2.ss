; Invalid syntax of the terminal extension
;   lang
;   (- some . terminals)
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((- some . terminals)) ) ))

; Unexpected dotted list in terminal extension
;   lang
;   (- some . terminals)
;   terminals
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((- some . terminals)) ) ))
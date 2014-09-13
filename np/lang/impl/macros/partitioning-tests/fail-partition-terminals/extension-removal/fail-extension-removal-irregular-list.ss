; Unexpected dotted list in terminal description
;   lang
;   (- dotted . list)
;   list
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((- dotted . list)) ) ))

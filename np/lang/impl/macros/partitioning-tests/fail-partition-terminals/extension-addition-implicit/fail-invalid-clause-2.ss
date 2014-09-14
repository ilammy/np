; Invalid terminal description syntax
;   lang
;   (number? (n nn) x)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((number? (n nn) x)) ) ))

; Unexpected dotted list in terminal extension
;   lang
;   (+ (number? (n)) . random)
;   random
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((+ (number? (n)) . random)) ) ))

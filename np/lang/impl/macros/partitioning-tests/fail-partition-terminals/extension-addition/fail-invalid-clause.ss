; Invalid syntax of the terminal
;   lang
;   (number number? (n nn) x)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((+ (number number? (n nn) x))) ) ))

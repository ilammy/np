; Invalid syntax of the terminal
;   lang
;   (num (a . d) (car . cdr))
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((- (num (a . d) (car . cdr)))) ) ))

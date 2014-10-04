; Unexpected dotted list in terminal definition
;   lang
;   num
;   (car . cdr)
;   cdr
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((- (num (a . d) (car . cdr)))) ) ))

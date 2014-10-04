; Name of the meta-variable must be a symbol
;   lang
;   num
;   (car . cdr)
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! (num ((- (car . cdr)))))) ) ))

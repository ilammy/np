; Invalid syntax of the terminal extension
;   lang
;   (nil car . cdr)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((nil car . cdr)) ) ))

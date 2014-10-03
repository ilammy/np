; Invalid syntax of the terminal
;   lang
;   a-symbol
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((+ a-symbol)) ) ))

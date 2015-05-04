; Invalid syntax of the terminal extension
;   lang
;   (+ . #(number? (n)))
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((+ . #(number? (n)))) ) ))

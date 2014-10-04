; Expected a list of terminal definitions
;   lang
;   (+ . #(number? (n)))
;   #(number? (n))
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((+ . #(number? (n)))) ) ))

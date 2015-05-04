; Invalid syntax of the terminal extension
;   lang
;   (+ . right-away)
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((+ . right-away)) ) ))

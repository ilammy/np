; Name of the terminal must be an identifier
;   lang
;   (11 ((+ some) (- vars)))
;   11
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! (11 ((+ some) (- vars))))) ) ))

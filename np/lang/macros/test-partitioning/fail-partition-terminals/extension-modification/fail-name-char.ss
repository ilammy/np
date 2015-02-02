; Name of the terminal must be an identifier
;   lang
;   (#\O ((+ some) (- vars)))
;   #\O
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! (#\O ((+ some) (- vars))))) ) ))

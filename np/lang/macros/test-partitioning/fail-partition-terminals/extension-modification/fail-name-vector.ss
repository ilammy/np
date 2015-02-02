; Name of the terminal must be an identifier
;   lang
;   (#(1) ((+ some) (- vars)))
;   #(1)
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! (#(1) ((+ some) (- vars))))) ) ))

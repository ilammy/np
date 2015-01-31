; Name of the terminal must be an identifier
;   lang
;   (#t ((+ some) (- vars)))
;   #t
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! (#t ((+ some) (- vars))))) ) ))

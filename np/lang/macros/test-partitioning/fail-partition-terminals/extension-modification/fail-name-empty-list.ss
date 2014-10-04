; Name of the terminal must be a symbol
;   lang
;   (() ((+ some) (- vars)))
;   ()
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! (() ((+ some) (- vars))))) ) ))

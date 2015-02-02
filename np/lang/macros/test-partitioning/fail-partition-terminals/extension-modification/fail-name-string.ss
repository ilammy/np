; Name of the terminal must be an identifier
;   lang
;   ("Eons" ((+ some) (- vars)))
;   "Eons"
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! ("Eons" ((+ some) (- vars))))) ) ))

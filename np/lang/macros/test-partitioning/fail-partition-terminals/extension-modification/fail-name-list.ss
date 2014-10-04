; Name of the terminal must be a symbol
;   lang
;   ((name) ((+ some) (- vars)))
;   (name)
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! ((name) ((+ some) (- vars))))) ) ))

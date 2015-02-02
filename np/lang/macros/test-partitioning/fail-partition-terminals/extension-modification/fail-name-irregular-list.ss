; Name of the terminal must be an identifier
;   lang
;   ((car . cdr) ((+ some) (- vars)))
;   (car . cdr)
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! ((car . cdr) ((+ some) (- vars))))) ) ))
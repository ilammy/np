; Name of the terminal must be an identifier
;   lang
;   (#\backspace predicate? (some vars))
;   #\backspace
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((- (#\backspace predicate? (some vars)))) ) ))

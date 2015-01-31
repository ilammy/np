; Name of the terminal must be an identifier
;   lang
;   (#\L predicate? (some vars))
;   #\L
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((+ (#\L predicate? (some vars)))) ) ))

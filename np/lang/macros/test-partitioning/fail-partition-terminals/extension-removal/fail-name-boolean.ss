; Name of the terminal must be an identifier
;   lang
;   (#f predicate? (some vars))
;   #f
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((- (#f predicate? (some vars)))) ) ))

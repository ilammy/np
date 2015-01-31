; Name of the terminal must be an identifier
;   lang
;   ("now" predicate? (some vars))
;   "now"
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((- ("now" predicate? (some vars)))) ) ))

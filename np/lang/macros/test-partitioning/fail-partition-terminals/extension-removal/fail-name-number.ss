; Name of the terminal must be an identifier
;   lang
;   (511 predicate? (some vars))
;   511
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((- (511 predicate? (some vars)))) ) ))

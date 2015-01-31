; Name of the terminal must be an identifier
;   lang
;   (0 predicate? (some vars))
;   0
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((+ (0 predicate? (some vars)))) ) ))

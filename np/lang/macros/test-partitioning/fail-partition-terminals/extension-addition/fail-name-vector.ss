; Name of the terminal must be an identifier
;   lang
;   (#() predicate? (some vars))
;   #()
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((+ (#() predicate? (some vars)))) ) ))

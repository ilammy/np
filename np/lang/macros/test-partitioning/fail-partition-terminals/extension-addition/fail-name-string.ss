; Name of the terminal must be an identifier
;   lang
;   ("are" predicate? (some vars))
;   "are"
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((+ ("are" predicate? (some vars)))) ) ))

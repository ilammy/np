; Name of the terminal must be a symbol
;   lang
;   ((name) predicate? (some vars))
;   (name)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((- ((name) predicate? (some vars)))) ) ))

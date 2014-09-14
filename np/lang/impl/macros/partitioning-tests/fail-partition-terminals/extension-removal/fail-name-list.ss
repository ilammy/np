; Terminal name must be a symbol
;   lang
;   ((name) predicate? (some vars))
;   (name)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((- ((name) predicate? (some vars)))) ) ))

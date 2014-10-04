; Terminal predicate must be a variable in short form
;   lang
;   ((a . d) (some vars))
;   (a . d)
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((+ ((a . d) (some vars)))) ) ))

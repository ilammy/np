; Terminal predicate must be a variable in short form
;   lang
;   (#(1 2 3) (some vars))
;   #(1 2 3)
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((+ (#(1 2 3) (some vars)))) ) ))
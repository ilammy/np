; Terminal predicate must be a variable in short form
;   lang
;   ((lambda (x) (odd? x)) (some vars))
;   (lambda (x) (odd? x))
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((- ((lambda (x) (odd? x)) (some vars)))) ) ))

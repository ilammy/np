; Terminal name must be a symbol
;   lang
;   ((car . cdr) predicate? (some vars))
;   (car . cdr)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((- ((car . cdr) predicate? (some vars)))) ) ))

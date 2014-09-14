; Predicate must be a symbol in short form
;   lang
;   ((a . d) (some vars))
;   (a . d)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((- ((a . d) (some vars)))) ) ))

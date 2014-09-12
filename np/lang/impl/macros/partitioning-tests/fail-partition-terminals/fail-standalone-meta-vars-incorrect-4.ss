; Meta-var name cannot be a list
;   lang
;   num
;   (car . cdr)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-descriptions 'lang
    '((num (car . cdr) ((car . cdr)))) ) ))

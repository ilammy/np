; Meta-variable name must be a symbol
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

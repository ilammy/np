; Name of the meta-variable must be a symbol
;   lang
;   num
;   (car . cdr)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-definitions 'lang
    '((num (car . cdr) ((car . cdr)))) ) ))

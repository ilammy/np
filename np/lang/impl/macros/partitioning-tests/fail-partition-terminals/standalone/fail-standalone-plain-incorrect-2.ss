; Invalid terminal description syntax
;   lang
;   (car . cdr)
;   ()
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-descriptions 'lang
    '((car . cdr) ()) ) ))

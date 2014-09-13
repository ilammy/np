; Invalid terminal description syntax
;   lang
;   (some invalid syntax)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-terminal-descriptions 'lang
    '((some invalid syntax)) ) ))

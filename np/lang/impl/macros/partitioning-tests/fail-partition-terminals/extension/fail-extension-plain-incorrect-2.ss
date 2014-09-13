; Invalid terminal description syntax
;   lang
;   (x)
;   (s . y)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((x) (s . y)) ) ))

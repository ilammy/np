; Invalid terminal description syntax
;   lang
;   omg
;   what
;   is
;   this
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '(omg what is this) ) ))

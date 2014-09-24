; Invalid extension meta-variable syntax
;   lang
;   num
;   ()
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((! (num ((- x) (+ y) ())))) ) ))

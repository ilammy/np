; Invalid syntax of the meta-variable modification
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

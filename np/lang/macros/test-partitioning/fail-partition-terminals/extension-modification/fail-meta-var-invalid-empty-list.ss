; Invalid syntax of the meta-variable modification
;   lang
;   num
;   ()
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! (num ((- x) (+ y) ())))) ) ))

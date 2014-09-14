; Invalid terminal description syntax
;   lang
;   (number? ((+ x) (- y)) x)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((number? ((+ x) (- y)) x)) ) ))

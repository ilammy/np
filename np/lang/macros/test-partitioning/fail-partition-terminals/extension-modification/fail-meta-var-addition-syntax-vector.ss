; Invalid syntax of the meta-variable modification
;   lang
;   number?
;   (+ . #())
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! (number? ((+ . #()))))) ) ))

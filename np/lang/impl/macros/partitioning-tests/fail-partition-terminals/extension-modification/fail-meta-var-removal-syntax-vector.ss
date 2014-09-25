; Expected a list of meta-variables
;   lang
;   number?
;   (- . #())
;   #()
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((! (number? ((- . #()))))) ) ))

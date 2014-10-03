; Expected a list of terminal definitions or names
;   lang
;   (- . #(number? (n)))
;   #(number? (n))
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((- . #(number? (n)))) ) ))

; Expected terminal description list
;   lang
;   (- . #(number? (n)))
;   #(number? (n))
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((- . #(number? (n)))) ) ))

; Invalid syntax of the nonterminal extension
;   lang
;   (- . #(Number pp (n)))
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((- . #(Number pp (n)))) ) ))

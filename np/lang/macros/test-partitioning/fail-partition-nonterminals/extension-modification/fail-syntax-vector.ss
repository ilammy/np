; Invalid syntax of the nonterminal extension
;   lang
;   (! . #(Number () (+ p)))
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! . #(Number () (+ p)))) ) ))

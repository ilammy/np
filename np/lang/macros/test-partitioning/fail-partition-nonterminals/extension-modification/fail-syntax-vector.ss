; Expected a list of nonterminal modifications
;   lang
;   (! . #(Number () (+ p)))
;   #(Number () (+ p))
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! . #(Number () (+ p)))) ) ))

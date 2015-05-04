; Expected a list of nonterminal definitions
;   lang
;   (+ . #(Number pp (n) p))
;   #(Number pp (n) p)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ . #(Number pp (n) p))) ) ))

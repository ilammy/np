; Expected a list of nonterminal definitions
;   lang
;   (+ . #(Number (n) p))
;   #(Number (n) p)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((+ . #(Number #(pp) (n) p))) ) ))

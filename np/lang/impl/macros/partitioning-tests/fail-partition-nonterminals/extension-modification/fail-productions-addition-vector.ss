; Incorrect production syntax: vector patterns are not allowed
;   lang
;   Nonterminal
;   (x #(p))
;   #(p)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((! (Nonterminal () (- (x #(p)))))) ) ))

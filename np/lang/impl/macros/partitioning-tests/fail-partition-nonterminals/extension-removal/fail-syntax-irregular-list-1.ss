; Unexpected dotted list in nonterminal extension
;   lang
;   (- (Number (n) n n) . random)
;   random
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((- (Number (n) n n) . random)) ) ))

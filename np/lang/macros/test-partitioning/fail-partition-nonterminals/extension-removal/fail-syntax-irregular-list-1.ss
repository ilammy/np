; Invalid syntax of the nonterminal extension
;   lang
;   (- (Number (n) n n) . random)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((- (Number (n) n n) . random)) ) ))

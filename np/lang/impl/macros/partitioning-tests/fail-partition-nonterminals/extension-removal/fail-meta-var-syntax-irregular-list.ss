; Unexpected dotted list in nonterminal description
;   lang
;   Number
;   (a . d)
;   d
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((- (Number (a . d) n))) ) ))

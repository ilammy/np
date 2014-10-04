; Unexpected dotted list in nonterminal definition
;   lang
;   Number
;   (a . d)
;   d
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (Number (a . d) n))) ) ))

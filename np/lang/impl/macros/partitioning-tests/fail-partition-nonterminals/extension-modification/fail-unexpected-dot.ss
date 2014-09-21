; Unexpected dotted list in nonterminal description
;   lang
;   (Pair ((+ x)) . right-away)
;   right-away
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((Pair ((+ x)) . right-away)) ) ))

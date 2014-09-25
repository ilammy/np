; Expected a list of nonterminal definitions or names
;   lang
;   (- . right-away)
;   right-away
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((- . right-away)) ) ))

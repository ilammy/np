; Expected a list of nonterminal modifications
;   lang
;   (! . right-away)
;   right-away
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! . right-away)) ) ))

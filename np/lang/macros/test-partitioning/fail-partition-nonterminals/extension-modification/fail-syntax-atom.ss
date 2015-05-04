; Invalid syntax of the nonterminal extension
;   lang
;   (! . right-away)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! . right-away)) ) ))

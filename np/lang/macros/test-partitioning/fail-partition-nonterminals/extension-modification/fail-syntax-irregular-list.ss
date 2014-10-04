; Unexpected dotted list in nonterminal extension
;   lang
;   (! (Number ((+ x))) . random)
;   random
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Number ((+ x))) . random)) ) ))

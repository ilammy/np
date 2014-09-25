; Unexpected dotted list in nonterminal extension
;   lang
;   (- some . nonterminals)
;   nonterminals
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((- some . nonterminals)) ) ))

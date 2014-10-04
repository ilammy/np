; Invalid syntax of the nonterminal
;   lang
;   (Number . HA-HA!)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((- (Number . HA-HA!))) ) ))

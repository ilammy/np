; Invalid syntax of the nonterminal
;   lang
;   (Number #(pred) . HA-HA!)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (Number #(pred) . HA-HA!))) ) ))

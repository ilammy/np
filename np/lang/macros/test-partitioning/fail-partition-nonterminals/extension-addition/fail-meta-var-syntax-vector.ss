; Invalid syntax of the nonterminal
;   lang
;   (Number a #(b) n)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (Number a #(b) n))) ) ))

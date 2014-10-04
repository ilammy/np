; Invalid syntax of the production: vector patterns are not allowed
;   lang
;   Nonterminal
;   #(vector!)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (Nonterminal () foo bar #(vector!)))) ) ))

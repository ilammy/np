; Invalid syntax of the production: vector patterns are not allowed
;   lang
;   Nonterminal
;   #(vector!)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((+ (Nonterminal () foo bar #(vector!)))) ) ))

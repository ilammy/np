; Invalid syntax of the production: vector patterns are not allowed
;   lang
;   Nonterminal
;   (some (deep list #(123)))
;   #(123)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((+ (Nonterminal () foo bar (some (deep list #(123)))))) ) ))

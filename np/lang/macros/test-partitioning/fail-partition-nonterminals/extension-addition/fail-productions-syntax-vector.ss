; Invalid syntax of the nonterminal
;   lang
;   (Nonterminal (nt) . #())
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (Nonterminal (nt) . #()))) ) ))

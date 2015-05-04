; Invalid syntax of the nonterminal
;   lang
;   (Nonterminal p () . sudden-atom)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (Nonterminal p () . sudden-atom))) ) ))

; Expected a list of productions
;   lang
;   Nonterminal
;   sudden-atom
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (Nonterminal p () . sudden-atom))) ) ))

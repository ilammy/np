; Nonterminal must have at least one production
;   lang
;   Nonterminal
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((+ (Nonterminal (nt)))) ) ))

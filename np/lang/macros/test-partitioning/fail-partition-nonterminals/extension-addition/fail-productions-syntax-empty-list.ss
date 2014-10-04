; At least one production should be specified for a nonterminal
;   lang
;   Nonterminal
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (Nonterminal (nt)))) ) ))

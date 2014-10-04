; Expected a list of meta-variable modifications
;   lang
;   Nonterminal
;   bar
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Nonterminal bar))) ) ))

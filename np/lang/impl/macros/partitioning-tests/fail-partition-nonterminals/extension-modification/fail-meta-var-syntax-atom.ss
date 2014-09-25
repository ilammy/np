; Expected a list of meta-variable modifications
;   lang
;   Nonterminal
;   bar
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((! (Nonterminal bar))) ) ))

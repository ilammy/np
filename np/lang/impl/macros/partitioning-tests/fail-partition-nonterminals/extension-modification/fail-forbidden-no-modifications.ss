; Nonterminal modification should modify either meta-variables or productions
;   lang
;   Nonterminal
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Nonterminal ()))) ) ))

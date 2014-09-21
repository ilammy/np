; Predicate name must be a symbol
;   lang
;   (Nonterminal #(1 2 3) ())
;   #(1 2 3)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((Nonterminal #(1 2 3) ())) ) ))

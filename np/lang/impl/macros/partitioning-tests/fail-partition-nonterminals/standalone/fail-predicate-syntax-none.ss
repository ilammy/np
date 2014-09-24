; Predicate name cannot be empty
;   lang
;   (Nonterminal #() ())
;   #()
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-descriptions 'lang
    '((Nonterminal #() () prod)) ) ))

; Nonterminal must have at least one production
;   lang
;   (Nonterminal (nt))
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-descriptions 'lang
    '((Nonterminal (nt))) ) ))

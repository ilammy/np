; Unexpected dotted list in nonterminal definition
;   lang
;   Nonterminal
;   (foo . bar)
;   bar
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-descriptions 'lang
    '((Nonterminal () foo . bar)) ) ))

; Incorrect production syntax: vector patterns are not allowed
;   lang
;   Nonterminal
;   #(vector!)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-descriptions 'lang
    '((Nonterminal () foo bar #(vector!))) ) ))

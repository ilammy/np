; Invalid syntax of the production: vector patterns are not allowed
;   lang
;   Nonterminal
;   (some (deep list #(123)))
;   #(123)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-definitions 'lang
    '((Nonterminal () foo bar (some (deep list #(123))))) ) ))

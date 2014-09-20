; Invalid nonterminal description syntax
;   lang
;   (Number . HA-HA!)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-descriptions 'lang
    '((Number . HA-HA!)) ) ))

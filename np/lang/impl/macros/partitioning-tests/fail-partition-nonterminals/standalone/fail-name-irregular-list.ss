; Nonterminal name must be a symbol
;   lang
;   ((car . cdr) #(pred?) ())
;   (car . cdr)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($filter-standalone-nonterminal-descriptions 'lang
    '(((car . cdr) #(pred?) () production1)) ) ))

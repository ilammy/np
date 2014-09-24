; Nonterminal name must be a symbol
;   lang
;   ((a . d) ((+ x)))
;   (a . d)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((! ((a . d) ((+ x)) (+ n)))) ) ))

; Name of the nonterminal must be an identifier
;   lang
;   ((a . d) ((+ x)))
;   (a . d)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! ((a . d) ((+ x)) (+ n)))) ) ))

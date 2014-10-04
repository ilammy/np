; Unexpected dotted list in nonterminal modification
;   lang
;   Pair
;   ((+ n (n n)) . foo)
;   foo
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Pair ((+ x)) (+ n (n n)) . foo))) ) ))

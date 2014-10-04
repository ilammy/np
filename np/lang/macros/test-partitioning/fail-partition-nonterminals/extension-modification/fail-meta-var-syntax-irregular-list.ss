; Unexpected dotted list in nonterminal modification
;   lang
;   Nonterminal
;   ((- x) . foo)
;   foo
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Nonterminal ((- x) . foo)))) ) ))

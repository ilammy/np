; Invalid syntax of the nonterminal modification
;   lang
;   (Pair ((+ x)) . foo)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Pair ((+ x)) . foo))) ) ))

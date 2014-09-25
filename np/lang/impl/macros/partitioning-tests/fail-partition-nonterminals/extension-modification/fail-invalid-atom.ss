; Invalid syntax of the nonterminal modification
;   lang
;   xyzzy
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((! xyzzy)) ) ))

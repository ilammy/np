; Invalid syntax of the production modification
;   lang
;   Pair
;   ()
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Pair ((+ x)) ()))) ) ))

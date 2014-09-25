; Invalid syntax of the meta-variable modification
;   lang
;   Pair
;   (y . z)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((! (Pair ((+ x) (y . z))))) ) ))

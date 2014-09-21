; Invalid extension production syntax
;   lang
;   Pair
;   (x y z)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((Pair ((+ x)) (x y z))) ) ))

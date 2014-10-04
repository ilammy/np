; Invalid syntax of the meta-variable modification
;   lang
;   Pair
;   atom
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Pair (atom (- x))))) ) ))

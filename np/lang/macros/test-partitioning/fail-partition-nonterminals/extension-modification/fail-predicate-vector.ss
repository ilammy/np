; Name of the nonterminal predicate must be an identifier
;   lang
;   Number
;   #(9)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Number #(9) ((+ x))))) ) ))

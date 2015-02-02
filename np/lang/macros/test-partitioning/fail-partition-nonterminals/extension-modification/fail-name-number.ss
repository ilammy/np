; Name of the nonterminal must be an identifier
;   lang
;   (9 ((+ vars)))
;   9
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (9 ((+ vars)) (+ prod)))) ) ))

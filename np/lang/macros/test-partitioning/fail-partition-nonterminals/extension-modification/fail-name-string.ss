; Name of the nonterminal must be an identifier
;   lang
;   ("up" ((+ vars)))
;   "up"
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! ("up" ((+ vars)) (+ prod)))) ) ))

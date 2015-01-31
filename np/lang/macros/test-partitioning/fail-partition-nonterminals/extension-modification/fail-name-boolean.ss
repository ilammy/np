; Name of the nonterminal must be an identifier
;   lang
;   (#t ((+ vars)))
;   #t
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (#t ((+ vars)) (+ prod)))) ) ))

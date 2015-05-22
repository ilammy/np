; Name of the nonterminal must be an identifier
;   lang
;   (#\f False? ((+ vars)))
;   #\f
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (#\f False? ((+ vars)) (+ prod)))) ) ))

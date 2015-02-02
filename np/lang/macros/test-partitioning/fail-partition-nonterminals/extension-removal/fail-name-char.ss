; Name of the nonterminal must be an identifier
;   lang
;   (#\G (some vars))
;   #\G
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((- (#\G (some vars)))) ) ))

; Name of the nonterminal must be an identifier
;   lang
;   (#\b (some vars))
;   #\b
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (#\b (some vars) production1))) ) ))

; Name of the nonterminal must be an identifier
;   lang
;   ("to" (some vars))
;   "to"
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ ("to" (some vars) production1))) ) ))

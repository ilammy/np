; Name of the meta-variable must be an identifier
;   lang
;   Number
;   "Look"
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (Number ("Look") n))) ) ))

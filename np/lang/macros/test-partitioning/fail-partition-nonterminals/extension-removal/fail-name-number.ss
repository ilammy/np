; Name of the nonterminal must be an identifier
;   lang
;   (4 (some vars))
;   4
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((- (4 (some vars)))) ) ))

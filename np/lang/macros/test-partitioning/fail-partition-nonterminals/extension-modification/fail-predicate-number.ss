; Name of the nonterminal predicate must be an identifier
;   lang
;   Number
;   42
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Number 42))) ) ))

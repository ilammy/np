; Name of the nonterminal predicate cannot be empty
;   lang
;   (Nonterminal #() ())
;   #()
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((- (Nonterminal #() ()))) ) ))

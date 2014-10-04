; Name of the nonterminal predicate must be a symbol
;   lang
;   (Nonterminal #((a . d)) ())
;   (a . d)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (Nonterminal #((a . d)) () "technically incorrect production"))) ) ))

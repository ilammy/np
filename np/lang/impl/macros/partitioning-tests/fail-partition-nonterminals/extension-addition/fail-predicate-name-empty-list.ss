; Name of the nonterminal predicate must be a symbol
;   lang
;   (Nonterminal #(()) (var))
;   ()
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((+ (Nonterminal #(()) (var) prod))) ) ))

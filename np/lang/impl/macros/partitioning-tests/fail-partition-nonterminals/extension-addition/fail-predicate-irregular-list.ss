; Predicate name must be a symbol
;   lang
;   (Nonterminal (a . d) ())
;   (a . d)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((+ (Nonterminal (a . d) () "technically incorrect production"))) ) ))

; Predicate name must be a symbol
;   lang
;   (Nonterminal (foo bar) ())
;   (foo bar)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((! (Nonterminal (foo bar) ()))) ) ))

; Only one predicate name is allowed
;   lang
;   (Nonterminal #(a b c) ())
;   #(a b c)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((+ (Nonterminal #(a b c) () "technically incorrect production"))) ) ))

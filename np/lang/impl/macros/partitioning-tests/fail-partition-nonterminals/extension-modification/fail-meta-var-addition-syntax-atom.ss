; Expected a list of meta-variables
;   lang
;   Foo
;   (+ . foo)
;   foo
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((! (Foo ((+ . foo))))) ) ))

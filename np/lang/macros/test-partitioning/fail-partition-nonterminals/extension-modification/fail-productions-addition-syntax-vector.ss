; Invalid syntax of the production modification
;   lang
;   Foo
;   (+ . #())
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Foo () (+ . #())))) ) ))

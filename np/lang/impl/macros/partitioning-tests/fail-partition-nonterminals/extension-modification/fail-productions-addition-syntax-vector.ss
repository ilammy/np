; Expected production list
;   lang
;   Foo
;   (+ . #())
;   #()
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((! (Foo () (+ . #())))) ) ))

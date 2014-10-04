; Unexpected dotted list in production modification
;   lang
;   Foo
;   (- p . x)
;   x
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Foo () (- p . x)))) ) ))

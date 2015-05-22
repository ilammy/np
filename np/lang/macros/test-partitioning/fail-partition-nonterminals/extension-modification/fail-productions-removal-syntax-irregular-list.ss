; Invalid syntax of the production modification
;   lang
;   Foo
;   (- p . x)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Foo () (- p . x)))) ) ))

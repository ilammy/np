; At least one production should be specified for addition
;   lang
;   Foo
;   (+)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Foo ((+ x)) (+)))) ) ))

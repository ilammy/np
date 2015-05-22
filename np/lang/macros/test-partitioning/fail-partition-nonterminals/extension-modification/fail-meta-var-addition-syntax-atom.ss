; Invalid syntax of the meta-variable modification
;   lang
;   Foo
;   (+ . foo)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Foo ((+ . foo))))) ) ))

; Unexpected dotted list in meta-variable modification
;   lang
;   Foo
;   (- car . cdr)
;   cdr
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Foo ((- car . cdr))))) ) ))

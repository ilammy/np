; Invalid syntax of the meta-variable modification
;   lang
;   Foo
;   (+ car . cdr)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Foo ((+ car . cdr))))) ) ))

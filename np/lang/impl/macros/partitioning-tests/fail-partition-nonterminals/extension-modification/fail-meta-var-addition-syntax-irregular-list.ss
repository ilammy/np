; Unexpected dotted list in meta-variable modification
;   lang
;   Foo
;   (+ car . cdr)
;   cdr
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((! (Foo ((+ car . cdr))))) ) ))

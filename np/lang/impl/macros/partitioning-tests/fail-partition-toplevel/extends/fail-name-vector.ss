; Name of the language to be extended must be a symbol
;   lang
;   (extends #(some-lang))
;   #(some-lang)
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((extends #(some-lang))) ) ))

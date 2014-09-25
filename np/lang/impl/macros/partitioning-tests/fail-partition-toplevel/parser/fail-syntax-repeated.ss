; Only one 'parser' clause can be specified
;   lang
;   (parser parse-lang)
;   (parser parse-lang)
;   (parser parse-lang)
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((parser parse-lang) (parser parse-lang) (parser parse-lang)) ) ))

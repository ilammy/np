; Only one 'unparser' clause can be specified
;   lang
;   (unparser unparse-lang)
;   (unparser unparse-lang)
(import (scheme base)
        (np lang impl macros partitioning-toplevel)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-toplevel-clauses 'lang
    '((unparser unparse-lang) (parser parse-lang) (unparser unparse-lang)) ) ))

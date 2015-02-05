; Expected a list of toplevel clauses
;   foo
;   #(bar)
(import (scheme base)
        (np lang macros define-language))

(define-language foo . #(bar))

; Meta-variable name must be a symbol
;   lang
;   Num
;   (+ (car . cdr))
;   (car . cdr)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((! (Num ((+ (car . cdr)))))) ) ))

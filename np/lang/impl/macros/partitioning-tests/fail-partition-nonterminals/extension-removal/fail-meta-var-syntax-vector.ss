; Expected a list of meta-variables
;   lang
;   Number
;   #(b)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((- (Number #(a) #(b) n))) ) ))

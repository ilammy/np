; Invalid nonterminal description syntax
;   lang
;   (Number #(pred))
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((- (Number #(pred)))) ) ))

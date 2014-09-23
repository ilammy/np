; Nonterminal name must be a symbol
;   lang
;   (#(9) ((+ vars)))
;   #(9)
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((! (#(9) ((+ vars)) (+ prod)))) ) ))

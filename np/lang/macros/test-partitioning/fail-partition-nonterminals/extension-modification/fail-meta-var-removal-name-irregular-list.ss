; Name of the meta-variable must be an identifier
;   lang
;   Number
;   (car . cdr)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Number ((- (car . cdr)))))) ) ))

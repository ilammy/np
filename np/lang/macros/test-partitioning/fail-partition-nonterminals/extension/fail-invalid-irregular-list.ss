; Invalid syntax of the nonterminal extension
;   lang
;   (nil car . cdr)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((nil car . cdr)) ) ))

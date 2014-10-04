; Expected a list of meta-variables
;   lang
;   Number
;   SURPRISE!
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (Number SURPRISE! n))) ) ))

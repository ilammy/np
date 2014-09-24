; Expected meta-variable list
;   lang
;   Number
;   SURPRISE!
(import (scheme base)
        (np lang impl macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-descriptions 'lang
    '((+ (Number SURPRISE! n))) ) ))

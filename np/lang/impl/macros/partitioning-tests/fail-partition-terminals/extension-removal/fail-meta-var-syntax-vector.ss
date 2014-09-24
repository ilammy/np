; Expected meta-variable list
;   lang
;   number?
;   #(1 2 3)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((- (number? #(1 2 3)))) ) ))

; Expected a list of meta-variables
;   lang
;   number?
;   #(1 2 3)
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((+ (number? #(1 2 3)))) ) ))

; Name of the meta-variable must be an identifier
;   lang
;   Num
;   4
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((+ (Num number? (4)))) ) ))
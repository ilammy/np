; Name of the meta-variable must be an identifier
;   lang
;   Num
;   222
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! (Num ((- 222))))) ) ))

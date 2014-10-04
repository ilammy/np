; Unexpected dotted list in meta-variable modification
;   lang
;   num
;   (- car . cdr)
;   cdr
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! (num ((- car . cdr))))) ) ))

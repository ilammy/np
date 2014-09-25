; Unexpected dotted list in meta-variable modification
;   lang
;   num
;   (- car . cdr)
;   cdr
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((! (num ((- car . cdr))))) ) ))

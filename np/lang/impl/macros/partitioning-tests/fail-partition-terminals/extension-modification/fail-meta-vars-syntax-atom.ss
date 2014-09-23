; Invalid extension meta-variable syntax
;   lang
;   num
;   atom
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((! (num (atom (- x))))) ) ))

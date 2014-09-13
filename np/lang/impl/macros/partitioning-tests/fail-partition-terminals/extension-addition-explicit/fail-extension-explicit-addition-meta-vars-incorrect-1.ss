; Meta-var name cannot be a vector
;   lang
;   Num
;   #(x)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((+ (Num number? (#(x))))) ) ))

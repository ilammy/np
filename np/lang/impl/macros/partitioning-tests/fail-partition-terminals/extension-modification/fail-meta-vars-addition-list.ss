; Meta-variable name must be a symbol
;   lang
;   number?
;   (+ (+ x))
;   (+ x)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((number? ((+ (+ x))))) ) ))

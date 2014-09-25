; Name of the terminal must be a symbol
;   lang
;   (() predicate? (some vars))
;   ()
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((+ (() predicate? (some vars)))) ) ))

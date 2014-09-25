; Invalid syntax of the terminal modification
;   lang
;   (name predicate? ((+ x) (- y)))
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((! (name predicate? ((+ x) (- y))))) ) ))

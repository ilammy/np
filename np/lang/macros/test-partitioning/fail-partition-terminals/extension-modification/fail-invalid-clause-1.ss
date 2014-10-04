; Invalid syntax of the terminal modification
;   lang
;   (name predicate? ((+ x) (- y)))
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! (name predicate? ((+ x) (- y))))) ) ))

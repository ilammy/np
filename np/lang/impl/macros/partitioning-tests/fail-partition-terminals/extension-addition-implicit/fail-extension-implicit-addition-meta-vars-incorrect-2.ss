; Meta-var name cannot be a list
;   lang
;   number?
;   (x y)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((number? ((x y)))) ) ))

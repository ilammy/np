; Invalid terminal description syntax
;   lang
;   #(symbol? (s))
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((- #(symbol? (s)))) ) ))

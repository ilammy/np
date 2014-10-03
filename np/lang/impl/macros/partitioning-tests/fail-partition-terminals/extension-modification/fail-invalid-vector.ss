; Invalid syntax of the terminal modification
;   lang
;   #(symbol? ((+ s)))
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! #(symbol? ((+ s))))) ) ))

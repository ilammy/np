; Expected a list of meta-variable modifications
;   lang
;   bar
;   #((+ x y z))
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! (bar #((+ x y z))))) ) ))
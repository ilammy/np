; Invalid terminal description syntax
;   lang
;   (#(1 2 3) (some vars))
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((+ (#(1 2 3) (some vars)))) ) ))

; Unexpected dotted list in terminal modification
;   lang
;   num
;   ((- x) . foo)
;   foo
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! (num ((- x) . foo)))) ) ))

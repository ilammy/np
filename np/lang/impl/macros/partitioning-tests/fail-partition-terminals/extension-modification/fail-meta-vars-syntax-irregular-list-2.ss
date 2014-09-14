; Unexpected dotted list in terminal description
;   lang
;   num
;   ((- x) . foo)
;   foo
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((num ((- x) . foo))) ) ))

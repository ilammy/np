; Terminal modification should modify meta-variables
;   lang
;   foo
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! (foo ()))) ) ))

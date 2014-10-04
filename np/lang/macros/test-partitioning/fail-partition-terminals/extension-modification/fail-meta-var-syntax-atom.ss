; Expected a list of meta-variable modifications
;   lang
;   foo
;   bar
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((! (foo bar))) ) ))

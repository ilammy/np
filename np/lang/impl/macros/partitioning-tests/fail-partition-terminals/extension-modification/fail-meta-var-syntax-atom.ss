; Expected meta-variable modification list
;   lang
;   foo
;   bar
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((! (foo bar))) ) ))

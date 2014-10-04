; Expected a list of terminal definitions or names
;   lang
;   (- . right-away)
;   right-away
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((- . right-away)) ) ))

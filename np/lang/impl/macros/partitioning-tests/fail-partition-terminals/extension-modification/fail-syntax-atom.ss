; Expected a list of terminal modifications
;   lang
;   (! . right-away)
;   right-away
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((! . right-away)) ) ))

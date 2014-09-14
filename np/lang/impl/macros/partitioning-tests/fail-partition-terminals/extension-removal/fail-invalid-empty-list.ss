; At least one terminal should be specified for removal
;   lang
;   (-)
(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-descriptions 'lang
    '((-)) ) ))

; Expected a list of meta-variables
;   lang
;   number?
;   foo
(import (scheme base)
        (np lang macros partitioning-terminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-terminal-definitions 'lang
    '((- (number? foo))) ) ))

; Invalid syntax of the nonterminal extension
;   lang
;   (some thing)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((some thing)) ) ))

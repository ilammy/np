; Name of the meta-variable must be an identifier
;   lang
;   Num
;   "way"
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Num ((- "way"))))) ) ))
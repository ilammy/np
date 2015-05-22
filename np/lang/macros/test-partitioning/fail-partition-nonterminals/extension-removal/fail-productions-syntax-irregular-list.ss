; Invalid syntax of the nonterminal
;   lang
;   (Nonterminal () foo . bar)
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((- (Nonterminal () foo . bar))) ) ))

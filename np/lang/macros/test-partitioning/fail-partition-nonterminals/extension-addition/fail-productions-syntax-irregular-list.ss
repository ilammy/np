; Unexpected dotted list in nonterminal definition
;   lang
;   Nonterminal
;   (foo . bar)
;   bar
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((+ (Nonterminal () foo . bar))) ) ))

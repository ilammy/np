; Expected a list of meta-variable modifications
;   lang
;   Nonterminal
;   #((+ x y z))
(import (scheme base)
        (np lang macros partitioning-nonterminals)
        (sr ck)
        (sr ck kernel))

($ ($quote
  ($partition-extension-nonterminal-definitions 'lang
    '((! (Nonterminal #((+ x y z))))) ) ))

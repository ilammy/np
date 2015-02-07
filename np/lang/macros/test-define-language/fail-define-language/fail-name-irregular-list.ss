; Name of the language must be an identifier
;   (foo . bar)
(import (scheme base)
        (np lang macros define-language))

(define-language (foo . bar))

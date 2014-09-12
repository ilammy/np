(import (scheme base)
        (np lang impl macros partitioning-terminals)
        (sr ck)
        (sr ck kernel)
        (te base)
        (te conditions assertions)
        (te utils verify-test-case))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-test-case (terminals:standalone "Filtering of standalone terminal forms")

  (define-test ("accepts full form")
    (assert-equal '((number number? (n)))
      ($ ($quote ($filter-standalone-terminal-descriptions 'lang
        '((number number? (n))) ))) ) )

  (define-test ("accepts short form")
    (assert-equal '((number? (n nn)))
      ($ ($quote ($filter-standalone-terminal-descriptions 'lang
        '((number? (n nn))) ))) ) )

  (define-test ("accepts expressions in full form")
    (assert-equal '((odd-number (lambda (x) (and (number? x) (odd? x))) (o-n)))
      ($ ($quote ($filter-standalone-terminal-descriptions 'lang
        '((odd-number (lambda (x) (and (number? x) (odd? x))) (o-n))) ))) ) )

  (define-test ("leaves clauses in order")
    (assert-equal '((terminal1 (v11 v12 v13))
                    (terminal2 (v21 v22 v23))
                    (terminal3 (v31 v32 v33)))
      ($ ($quote ($filter-standalone-terminal-descriptions 'lang
        '((terminal1 (v11 v12 v13))
          (terminal2 (v21 v22 v23))
          (terminal3 (v31 v32 v33))) ))) ) )

  (define-test ("accepts peculiar extension-like forms")
    (assert-equal '((+ (Look like (extension forms))
                    (while in fact)) (- (they are not)))
      ($ ($quote ($filter-standalone-terminal-descriptions 'lang
        '((+ (Look like (extension forms)) (while in fact))
          (- (they are not))) ))) ) )
)
(verify-test-case! terminals:standalone)

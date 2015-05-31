(define-library (np lang descriptions unification)
  ;;;
  ;;; Term normalization and unification
  ;;;
  (export trim-meta-var-name)

  (import (scheme base))

  (begin
    ;;;
    ;;; Name trimming
    ;;;

    (define (trim-meta-var-name name)
      (string->symbol
       (let ((name (symbol->string name)))
         ;
         ; s/^(.*?)[0-9]*[?*+^]*$/\1/
         ;
         (define (digit? c)
           (char<=? #\0 c #\9) )

         (define (special? c)
           (or (char=? #\? c) (char=? #\* c)
               (char=? #\+ c) (char=? #\^ c) ) )

         (define (scan-special i)
           (if (< i 0) ""
               (if (special? (string-ref name i))
                   (scan-special (- i 1))
                   (scan-digits i) ) ) )

         (define (scan-digits i)
           (if (< i 0) ""
               (if (digit? (string-ref name i))
                   (scan-digits (- i 1))
                   (substring name 0 (+ i 1)) ) ) )

         (scan-special (- (string-length name) 1)) ) ) )

) )

(define-library (np lang descriptions unification)
  ;;;
  ;;; Term normalization and unification
  ;;;
  (export trim-meta-var-name
          make-meta-var-mapping
          equality-for-mapping
          meta-var-mapped?)

  (import (scheme base)
          (srfi 69) ; hash tables
          (np lang descriptions definitions))

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

    ;;;
    ;;; Productions
    ;;;

    (define (make-meta-var-mapping terminals nonterminals)
      (let ((hash (make-hash-table eq? hash-by-identity)))
        (for-each
          (lambda (terminal)
            (for-each
              (lambda (meta-var) (hash-table-set! hash meta-var terminal))
              (terminal-meta-variables terminal) ) )
          terminals )
        (for-each
          (lambda (nonterminal)
            (for-each
              (lambda (meta-var) (hash-table-set! hash meta-var nonterminal))
              (nonterminal-meta-variables nonterminal) ) )
          nonterminals )
        hash ) )

    (define (equality-for-mapping mapping)
      (define (productions-equal? p1 p2)
        (cond ((and (null? p1) (null? p2)) #t)
              ((and (symbol? p1) (symbol? p2))
               (symbols-equal? mapping p1 p2) )
              ((and (pair? p1) (pair? p2))
               (and (productions-equal? (car p1) (car p2))
                    (productions-equal? (cdr p1) (cdr p2)) ) )
              (else #f) ) )
      productions-equal? )

    (define (symbols-equal? mapping s1 s2)
      (if (eq? s1 s2) ; If symbols are equal
          #t ; they are either equal literals, or obviously equal meta-variables
          (let ((e1 (hash-table-ref/default mapping (trim-meta-var-name s1) #f))
                (e2 (hash-table-ref/default mapping (trim-meta-var-name s2) #f)))
            (if (and e1 e2) ; If both symbols denote meta-variables
                (eq? e1 e2) ; they must refer to the same entity.
                #f ) ) ) )  ; Otherwise, one of them is literal and the other
                            ; one is a meta-variable or some different literal

    (define (meta-var-mapped? mapping x)
      (hash-table-exists? mapping (trim-meta-var-name x)) )

) )

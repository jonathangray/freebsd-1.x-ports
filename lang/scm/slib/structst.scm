(require 'struct)

(define-record foo (a b c))
(define-record goo (xx yy))

(define a-foo (make-foo 1 2 3))
(define a-goo (make-goo 4 5))

(define (struct:test)
  (define (t1 x)
    (variant-case x
      (foo (a b c) (list a b c))
      (goo (xx yy) (list xx yy))
      (else (list 7 8))))
  (write (append (t1 a-foo) (t1 a-goo) (t1 9)))
  (newline))

(struct:test)

;;;; "multapply.scm" Redefine APPLY take more than 2 arguments.

(define two-arg:apply apply)
(set! apply
      (lambda args
	(two-arg:apply (car args) (apply:append-to-last (cdr args)))))

(define (apply:append-to-last lst)
  (if (null? (cdr lst))
      (car lst)
      (cons (car lst) (apply:append-to-last (cdr lst)))))

;;;; "multarg.scm" Redefine - and / to take more than 2 arguments.
;;; From: hugh@ear.mit.edu (Hugh Secker-Walker)

;;; redefine / to take more than two arguments
(define two-arg:/ /)
(set! / (lambda (dividend . divisors)
	  (cond ((null? divisors) (two-arg:/ dividend))
		((null? (cdr divisors))
		 (two-arg:/ dividend (car divisors)))
		(else 
		 (for-each (lambda (divisor)
			     (set! dividend (two-arg:/ dividend divisor)))
			   divisors)
		 dividend))))

;;; redefine - to take more than two arguments
(define two-arg:- -)
(set! - (lambda (minuend . subtrahends)
	  (cond ((null? subtrahends) (two-arg:- minuend))
		((null? (cdr subtrahends))
		 (two-arg:- minuend (car subtrahends)))
		(else 
		 (for-each (lambda (subtrahend)
			     (set! minuend (two-arg:- minuend subtrahend)))
			   subtrahends)
		 minuend))))

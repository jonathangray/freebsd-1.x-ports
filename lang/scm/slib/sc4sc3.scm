;;;; Implementation of rev4 procedures for rev3.
;;; Copyright (C) 1991 Aubrey Jaffer.

;;;; peek-char, number->string, and string->number need to be written here.

;;; APPEND, +, *, -, /, =, <, >, <=, >=, MAP, and FOR-EACH need to
;;; accept more general number or arguments.

(define (list? x)
  (let loop ((fast x) (slow x))
    (or (null? fast)
	(and (pair? fast)
	     (let ((fast (cdr fast)))
	       (or (null? fast)
		   (and (pair? fast) 
			(let ((fast (cdr fast))
			      (slow (cdr slow)))
			  (and (not (eq? fast slow))
			       (loop fast slow))))))))))

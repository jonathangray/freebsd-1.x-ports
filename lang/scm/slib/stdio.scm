;;;; Implementation of <stdio.h> functions for Scheme
;;; Copyright (C) 1991-1993 Aubrey Jaffer.

;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.

;1.  Any copy made of this software must include this copyright notice
;in full.

;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.

;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;; Floating point is not handled yet.  It should not be hard to do.

(define (stdio:iprintf out format . args)
  (let loop ((pos 0) (args args))
    (if (< pos (string-length format))
	(case (string-ref format pos)
	  ((#\\ )
	   (set! pos (+ pos 1))
	   (case (string-ref format pos)
	     ((#\n #\N) (out #\newline))
	     ((#\t #\T) (out slib:tab))
	     ((#\r #\R) (out #\return))
	     ((#\f #\F) (out slib:form-feed))
	     (else (out (string-ref format pos))))
	   (loop (+ pos 1) args))
	  ((#\%)
	   (set! pos (+ pos 1))
	   (letrec ((left-adjust #f)
		    (pad-char 
		     (if (char=? #\0 (string-ref format pos)) #\0 #\ ))
		    (width 0)
		    (prec #f)
		    (pad
		     (lambda (s)
		       (cond ((<= width (string-length s)) s)
			     (left-adjust
			      (string-append
			       s
			       (make-string (- width (string-length s))
					    #\ )))
			     (else
			      (string-append
			       (make-string (- width (string-length s))
					    pad-char)
			       s))))))
	     (let ilp ((pos pos))
	       (case (string-ref format pos)
		 ((#\d #\D #\u #\U)
		  (out (pad (cond ((symbol? (car args))
				   (symbol->string (car args)))
				  ((number? (car args))
				   (number->string (car args)))
				  ((not (car args)) "0")
				  (else "1"))))
		  (loop (+ pos 1) (cdr args)))
		 ((#\c #\C)
		  (out (pad (string (car args))))
		  (loop (+ pos 1) (cdr args)))
		 ((#\o #\O)
		  (out (pad (number->string (car args) 8)))
		  (loop (+ pos 1) (cdr args)))
		 ((#\x #\X)
		  (out (pad (number->string (car args) 16)))
		  (loop (+ pos 1) (cdr args)))
		 ((#\l #\L) (ilp (+ pos 1)))
		 ((#\-) (set! left-adjust #t)
			(ilp (+ pos 1)))
		 ((#\.)
		  (set! prec 0)
		  (set! pos (+ 1 pos))
		  (let iilp ()
		    (case (string-ref format pos)
		      ((#\*) (set! prec (car args))
			     (set! args (cdr args))
			     (ilp (+ pos 1)))
		      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		       (set! prec
			     (+ (* prec 10)
				(- (char->integer (string-ref format pos))
				   (char->integer #\0))))
		       (set! pos (+ 1 pos))
		       (iilp))
		      (else (ilp pos)))))
		 ((#\%) (out #\%)
			(loop (+ pos 1) args))
		 ((#\s #\S)
		  (if (or (not prec)
			  (>= prec (string-length (car args))))
		      (out (pad (car args)))
		      (out (pad (substring (car args) 0 prec))))
		  (loop (+ pos 1) (cdr args)))
		 ((#\*) (set! width (car args))
			(set! args (cdr args))
			(ilp (+ pos 1)))
		 ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		  (set! width (+ (* width 10)
				 (- (char->integer (string-ref format pos))
				    (char->integer #\0))))
		  (ilp (+ pos 1)))
		 (else (out #\%)
		       (out (string-ref format pos))
		       (loop (+ pos 1) args))))))
	  (else (out (string-ref format pos))
		(loop (+ pos 1) args))))))

(define (stdio:printf format . args)
  (apply stdio:iprintf display format args))

(define (stdio:fprintf port format . args)
  (if (equal? port (current-output-port))
      (apply stdio:iprintf display format args)
      (apply stdio:iprintf (lambda (x) (display x port)) format args)))

(define (stdio:sprintf s format . args)
  (let ((p 0))
    (apply stdio:iprintf
	   (lambda (x)
	     (cond ((string? x)
		    (do ((i 0 (+ i 1)))
			((>= i (string-length x)))
		      (string-set! s p (string-ref x i))
		      (set! p (+ p 1))))
		   ((char? x)
		    (string-set! s p x)
		    (set! p (+ p 1)))
		   (else
		    (string-set! s p #\?)
		    (set! p (+ p 1)))))
	   format
	   args)
    p))

(define printf stdio:printf)
(define fprintf stdio:fprintf)
(define sprintf stdio:sprintf)

(define stdin (current-input-port))
(define stdout (current-output-port))
(define stderr (current-error-port))

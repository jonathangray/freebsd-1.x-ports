;;;; "dwindtst.scm", routines for characterizing dynamic-wind.
;;; Copyright (C) 1992 Aubrey Jaffer.

(require 'dynamic-wind)

(define (dwtest n)
  (define cont #f)
  (display "testing escape from thunk") (display n) (newline)
  (display "visiting:") (newline)
  (call-with-current-continuation
   (lambda (x) (set! cont x)))
  (if n
      (dynamic-wind
       (lambda ()
	 (display "thunk1") (newline)
	 (if (eqv? n 1) (let ((ntmp n))
			  (set! n #f)
			  (cont ntmp))))
       (lambda ()
	 (display "thunk2") (newline)
	 (if (eqv? n 2) (let ((ntmp n))
			  (set! n #f)
			  (cont ntmp))))
       (lambda ()
	 (display "thunk3") (newline)
	 (if (eqv? n 3) (let ((ntmp n))
			  (set! n #f)
			  (cont ntmp)))))))
(define (dwctest n)
  (define cont #f)
  (define ccont #f)
  (display "creating continuation thunk") (newline)
  (display "visiting:") (newline)
  (call-with-current-continuation
   (lambda (x) (set! cont x)))
  (if n (set! n (- n)))
  (if n
      (dynamic-wind
       (lambda ()
	 (display "thunk1") (newline)
	 (if (eqv? n 1) (let ((ntmp n))
			  (set! n #f)
			  (cont ntmp))))
       (lambda ()
	 (call-with-current-continuation
	  (lambda (x) (set! ccont x)))
	 (display "thunk2") (newline)
	 (if (eqv? n 2) (let ((ntmp n))
			  (set! n #f)
			  (cont ntmp))))
       (lambda ()
	 (display "thunk3") (newline)
	 (if (eqv? n 3) (let ((ntmp n))
			  (set! n #f)
			  (cont ntmp))))))
  (cond
   (n
    (set! n (- n))
    (display "testing escape from continuation thunk") (display n) (newline)
    (display "visiting:") (newline)
    (ccont #f))))

(dwtest 1) (dwtest 2) (dwtest 3)
(dwctest 1) (dwctest 2) (dwctest 3)

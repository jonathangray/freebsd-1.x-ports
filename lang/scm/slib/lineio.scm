; "lineio", line oriented input/output functions for Scheme.
; Copyright (c) 1992, 1993 Aubrey Jaffer

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

(define (read-line . arg)
  (let* ((char (apply read-char arg)))
    (if (eof-object? char)
	char
	(do ((char char (apply read-char arg))
	     (clist '() (cons char clist)))
	    ((or (eof-object? char) (char=? #\newline char))
	     (list->string (reverse clist)))))))

(define (read-line! str . arg)
  (let* ((char (apply read-char arg))
	 (len (+ -1 (string-length str))))
    (if (eof-object? char)
	char
	(do ((char char (apply read-char arg))
	     (i 0 (+ 1 i)))
	    ((or (eof-object? char)
		 (char=? #\newline char)
		 (>= i len))
	     (cond ((or (eof-object? char) (char=? #\newline char))
		    i)
		   (else
		    (string-set! str i char)
		    (set! char (apply peek-char arg))
		    (if (or (eof-object? char) (char=? #\newline char))
			(+ 1 i) #f))))
	  (string-set! str i char)))))

(define (write-line str . arg)
  (apply display str arg)
  (apply newline arg))

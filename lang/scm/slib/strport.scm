;;;;"strport.scm" Portable string ports for Scheme
;;;Copyright 1993 Dorai Sitaram and Aubrey Jaffer.

;N.B.: This implementation assumes you have tmpnam and
;delete-file defined in your .init file.  tmpnam generates
;temp file names.  delete-file may be defined to be a dummy
;procedure that does nothing.

(define (call-with-output-string f)
  (let ((tmpf (tmpnam)))
    (call-with-output-file tmpf f)
    (let ((s "") (buf (make-string 512)))
      (call-with-input-file tmpf
	(lambda (inp)
	  (let loop ((i 0))
	    (let ((c (read-char inp)))
	      (cond ((eof-object? c)
		     (set! s (string-append s (substring buf 0 i))))
		    ((>= i 512)
		     (set! s (string-append s buf))
		     (loop 0))
		    (else
		     (string-set! buf i c)
		     (loop (+ i 1))))))))
      (delete-file tmpf)
      s)))

(define (call-with-input-string s f)
  (let ((tmpf (tmpnam)))
    (call-with-output-file tmpf
      (lambda (outp)
	(display s outp)))
    (let ((x (call-with-input-file tmpf f)))
      (delete-file tmpf)
      x)))

;;;; "ppfile.scm".  Pretty print a Scheme file.
;
;  (pprint-file ifile ofile)				procedure
;
;Pretty-prints all the Scheme code in ifile to ofile.
;
;  (pprint-file ifile)					procedure
;
;Pretty-prints all the Scheme code in ifile.
;
(require 'pretty-print)

(define (pprint-file ifile . optarg)
  (let ((lst (call-with-input-file ifile
	       (lambda (iport)
		 (do ((obj (read iport) (read iport))
		      (lst '() (cons obj lst)))
		     ((eof-object? obj) lst))))))
    (if (null? optarg)
	(for-each pretty-print (reverse lst))
	(call-with-output-file (car optarg)
	  (lambda (oport)
	    (for-each (lambda (x) (pretty-print x oport))
		      (reverse lst)))))))

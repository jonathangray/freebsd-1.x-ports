;From Revised^4 Report on the Algorithmic Language Scheme
;William Clinger and Jonathon Rees (Editors)

(define promise:force (lambda (object) (object)))

(define make-promise
  (lambda (proc)
    (let ((result-ready? #f)
	  (result #f))
      (lambda ()
	(if result-ready?
	    result
	    (let ((x (proc)))
	      (if result-ready?
		  result
		  (begin (set! result-ready? #t)
			 (set! result x)
			 result))))))))

;;; change occurences of (DELAY <expression>) to
;;; (MAKE-PROMISE (LAMBDA () <expression>))
;;; and (define force promise:force)

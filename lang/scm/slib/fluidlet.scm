; "fluidlet.scm", FLUID-LET for Scheme
; Copyright (c) 1992, Dorai Sitaram (dorai@cs.rice.edu)

(require 'rev4-optional-procedures)
(require 'common-list-functions)
(require 'dynamic-wind)
(require 'macro)

(define list-set! (lambda (s i v) (set-car! (list-tail s i) v)))

(define-syntax fluid-let
  (syntax-rules ()
    ((fluid-let ((x v) ...) . body)
     (let ((%x-names (list 'x ...))
	   (%x-values (list x ...))
	   (%fluid-x-values (list v ...)))
       (dynamic-wind
	 (lambda ()
           (set! x (list-ref %fluid-x-values
		       (comlist:position 'x %x-names)))
           ...)
	 (lambda () . body)
	 (lambda ()
           (let ((%x-position (comlist:position 'x %x-names)))
             (list-set! %fluid-x-values %x-position x)
	     (set! x (list-ref %x-values %x-position)))
	   ...))))))

;--- end of file

;;; -*- Scheme -*-

(require 'eval)
(require 'common-list-functions)	;to pick up EVERY
(define syncase:andmap comlist:every)

; In Chez Scheme "(syncase:void)" returns an object that is ignored by the
; REP loop.  It is returned whenever a "nonspecified" value is specified
; by the standard.  The following should pick up an appropriate value.

(define syncase:void
   (let ((syncase:void-object (if #f #f)))
      (lambda () syncase:void-object)))

(define syncase:eval-hook slib:eval)

(define syncase:error-hook slib:error)

(define syncase:new-symbol-hook
  (let ((c 0))
    (lambda (string)
      (set! c (+ c 1))
      (string->symbol
       (string-append string ":Sca" (number->string c))))))

(define syncase:put-global-definition-hook #f)
(define syncase:get-global-definition-hook #f)
(let ((*macros* '()))
  (set! syncase:put-global-definition-hook
	(lambda (symbol binding)
	  (let ((pair (assq symbol *macros*)))
	    (if pair
		(set-cdr! pair binding)
		(set! *macros* (cons (cons symbol binding) *macros*))))))
  (set! syncase:get-global-definition-hook
	(lambda (symbol)
	  (let ((pair (assq symbol *macros*)))
	    (and pair (cdr pair))))))


;;;! expand.pp requires list*
(define (syncase:list* . args)
  (if (null? args)
      '()
      (let ((r (reverse args)))
	(append (reverse (cdr r))
		(car r)			; Last arg
		'()))))			; Make sure the last arg is copied

(define syntax-error syncase:error-hook)
(define impl-error slib:error)

(define base:eval eval)
(define syncase:eval base:eval)
(define macro:eval base:eval)
(define syncase:expand #f)
(define macro:expand #f)
(define (syncase:expand-install-hook expand)
  (set! syncase:eval (lambda (x) (base:eval (expand x))))
  (set! macro:eval syncase:eval)
  (set! syncase:expand expand)
  (set! macro:expand syncase:expand))
;;; We Need This for bootstrapping purposes:
(define (syncase:load <pathname>)
  (call-with-input-file <pathname>
    (lambda (port)
      (let ((old-load-pathname *load-pathname*))
	(set! *load-pathname* <pathname>)
	(do ((o (read port) (read port)))
	    ((eof-object? o))
	  (macro:eval o))
	(set! *load-pathname* old-load-pathname)))))
(define macro:load syncase:load)

(define syncase:sanity-check #f)
;;; LOADING THE SYSTEM ITSELF:
(let ((here (lambda (file)
	      (in-vicinity (library-vicinity) file (scheme-file-suffix)))))
  (for-each (lambda (file) (load (here file)))
	    '("scaoutp"
	      "scaglob"
	      "scaexpp"))
  (syncase:expand-install-hook expand-syntax)
  (macro:load (here
	       "scamacr"))
  (set! syncase:sanity-check
	(lambda ()
	  (macro:load (here "sca-exp"))
	  (syncase:expand-install-hook expand-syntax)
	  (macro:load (here "sca-macr")))))

(provide 'syntax-case)
(provide 'macro)

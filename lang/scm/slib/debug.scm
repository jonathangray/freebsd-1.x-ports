;;;; Utility functions for debugging in Scheme.
;;; Copyright (C) 1991, 1992, 1993 Aubrey Jaffer.

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

(define (debug:print . args)
  (define result #f)
  (for-each (lambda (x) (set! result x) (write x) (display #\ )) args)
  (newline)
  result)

(define *qp-width* (output-port-width (current-output-port)))

(define debug:qpn
  (let ((newline newline) (apply apply))
    (lambda objs (apply debug:qp objs) (newline))))

(define debug:qpr
  (let ((- -) (apply apply) (length length) (list-ref list-ref))
    (lambda objs (apply debug:qpn objs)
	    (list-ref objs (- (length objs) 1)))))

(define debug:qp
  (let
      ((+ +) (- -) (< <) (= =) (>= >=) (apply apply) (boolean? boolean?)
       (car car) (cdr cdr) (char? char?) (display display) (eq? eq?)
       (for-each for-each) (input-port? input-port?)
       (not not) (null? null?) (number->string number->string)
       (number? number?) (output-port? output-port?) (eof-object? eof-object?)
       (procedure? procedure?) (string-length string-length)
       (string? string?) (substring substring)
       (symbol->string symbol->string) (symbol? symbol?)
       (vector-length vector-length) (vector-ref vector-ref)
       (vector? vector?) (write write) (quotient quotient))
    (letrec
	((num-cdrs
	  (lambda (pairs max-cdrs)
	    (cond
	     ((null? pairs) 0)
	     ((< max-cdrs 1) 1)
	     ((pair? pairs) (+ 1 (num-cdrs (cdr pairs) (- max-cdrs 1))))
	     (else 1))))
	 
	 (l-elt-room
	  (lambda (room pairs)
	    (quotient room (num-cdrs pairs (quotient room 8)))))

	 (qp-pairs
	  (lambda (cdrs room)
	    (cond
	     ((null? cdrs) 0)
	     ((not (pair? cdrs))
	      (display " . ")
	      (+ 3 (qp-obj cdrs (l-elt-room (- room 3) cdrs))))
	     ((< 11 room)
	      (display #\ )
	      ((lambda (used)
		 (+ (qp-pairs (cdr cdrs) (- room used)) used))
	       (+ 1 (qp-obj (car cdrs) (l-elt-room (- room 1) cdrs)))))
	     (else
	      (display " ...") 4))))

	 (v-elt-room
	  (lambda (room vleft)
	    (quotient room (min vleft (quotient room 8)))))

	 (qp-vect
	  (lambda (vect i room)
	    (cond
	     ((= (vector-length vect) i) 0)
	     ((< 11 room)
	      (display #\ )
	      ((lambda (used)
		 (+ (qp-vect vect (+ i 1) (- room used)) used))
	       (+ 1 (qp-obj (vector-ref vect i)
			    (v-elt-room (- room 1)
					(- (vector-length vect) i))))))
	     (else
	      (display " ...") 4))))

	 (qp-string
	  (lambda (str room)
	    (cond
	     ((>= (string-length str) room 3)
	      (display (substring str 0 (- room 3)))
	      (display "...")
	      room)
	     (else
	      (display str)
	      (string-length str)))))

	 (qp-obj
	  (lambda (obj room)
	    (cond
	     ((null? obj) (write obj) 2)
	     ((boolean? obj) (write obj) 2)
	     ((char? obj) (write obj) 8)
	     ((number? obj) (qp-string (number->string obj) room))
	     ((string? obj)
	      (display #\")
	      ((lambda (ans) (display #\") ans)
	       (+ 2 (qp-string obj (- room 2)))))
	     ((symbol? obj) (qp-string (symbol->string obj) room))
	     ((input-port? obj) (display "#[input]") 8)
	     ((output-port? obj) (display "#[output]") 9)
	     ((procedure? obj) (display "#[proc]") 7)
	     ((eof-object? obj) (display "#[eof]") 6)
	     ((vector? obj)
	      (set! room (- room 3))
	      (display "#(")
	      ((lambda (used) (display #\)) (+ used 3))
	       (cond
		((= 0 (vector-length obj)) 0)
		((< room 8) (display "...") 3)
		(else
		 ((lambda (used) (+ (qp-vect obj 1 (- room used)) used))
		  (qp-obj (vector-ref obj 0)
			  (v-elt-room room (vector-length obj))))))))
	     ((pair? obj) 
	      (set! room (- room 2))
	      (display #\()
	      ((lambda (used) (display #\)) (+ 2 used))
	       (if (< room 8) (begin (display "...") 3)
		   ((lambda (used)
		      (+ (qp-pairs (cdr obj) (- room used)) used))
		    (qp-obj (car obj) (l-elt-room room obj))))))
	     (else (display "#[unknown]") 10)))))

      (lambda objs
	(qp-pairs (cdr objs)
		  (- *qp-width*
		     (qp-obj (car objs) (l-elt-room *qp-width* objs))))))))

(define debug:indent 0)

(define debug:tracef
  (let ((null? null?)			;These bindings are so that
	(not not)			;tracef will not trace parts
	(car car) (cdr cdr)		;of itself.
	(eq? eq?) (+ +) (zero? zero?) (modulo modulo)
	(apply apply) (display display) (qpn debug:qpn))
    (lambda (function . optname)
      (set! debug:indent 0)
      (let ((name (if (null? optname) function (car optname))))
	(lambda args
	  (cond ((and (not (null? args))
		      (eq? (car args) 'debug:untrace-object)
		      (null? (cdr args)))
		 function)
		(else
		 (do ((i debug:indent (+ -1 i))) ((zero? i)) (display #\ ))
		 (apply qpn "CALLED" name args)
		 (set! debug:indent (modulo (+ 1 debug:indent) 8))
		 (let ((ans (apply function args)))
		   (set! debug:indent (modulo (+ -1 debug:indent) 8))
		   (do ((i debug:indent (+ -1 i))) ((zero? i)) (display #\ ))
		   (qpn "RETURNED" name ans)
		   ans))))))))

;;; the reason I use a symbol for debug:untrace-object is so
;;; that functions can still be untraced if this file is read in twice.

(define (debug:untracef function)
  (set! debug:indent 0)
  (function 'debug:untrace-object))

;;;; BREAKPOINTS

;;; Typing (init-debug) at top level sets up a continuation for break.
;;; When (break arg1 ...) is then called it returns from the top level
;;; continuation and pushes the continuation from which it was called
;;; on debug:break-continuation-stack.  If (continue) is later
;;; called, it pops the topmost continuation off of
;;; debug:break-continuation-stack and returns #f to it.

(define debug:break-continuation-stack '())

(define debug:break
  (let ((call-with-current-continuation call-with-current-continuation)
	(apply apply) (qpn debug:qpn)
	(cons cons) (length length))
    (lambda args
      (apply qpn "BREAK:" args)
      (call-with-current-continuation
       (lambda (x) 
	 (set! debug:break-continuation-stack
	       (cons x debug:break-continuation-stack))
	 (debug:top-continuation
	  (length debug:break-continuation-stack)))))))

(define debug:continue
  (let ((null? null?) (car car) (cdr cdr))
    (lambda ()
      (cond ((null? debug:break-continuation-stack) #f)
	    (else
	     (let ((cont (car debug:break-continuation-stack)))
	       (set! debug:break-continuation-stack
		     (cdr debug:break-continuation-stack))
	       (cont #f)))))))

(define debug:top-continuation
  (if (provided? 'abort)
      (lambda (val) (display val) (newline) (abort))
      (begin (display "; type (init-debug)") #f)))

(define (init-debug)
  (call-with-current-continuation
   (lambda (x) (set! debug:top-continuation x))))

(define print debug:print)
(define qp debug:qp)
(define qpn debug:qpn)
(define qpr debug:qpr)
(define tracef debug:tracef)
(define untracef debug:untracef)
(define break debug:break)
(define continue debug:continue)

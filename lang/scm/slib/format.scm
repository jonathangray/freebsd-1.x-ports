;;;; Common LISP like text output formatter for R4RS Scheme
;
; Copyright (C) 1992, 1993 by Dirk Lutzebaeck (lutzeb@cs.tu-berlin.de)
;
; Authors of the original version (< 1.4) were Ken Dickey and Aubrey Jaffer.
; Please send error reports to the email address above.
;
; VERSION 2.3

;;; SCHEME IMPLEMENTATION DEPENDENCIES ---------------------------------------

(define format:scheme-system 'slib)

;; To configure the format module for your scheme system, set the variable
;; format:scheme-system to one of the symbols of '(slib scm elk mit any).
;; You may add appropiate definitions in the cond-construct below for
;; other systems. 

;; Default configuration definitions which may be overridden (see below).

(define format:abort #f)
;; Aborts the program when a formatting error occures. This is a null
;; argument closure to jump to the interpreters toplevel continuation.
;; If format:abort is #f then a continuation is used to recover properly
;; from a format error. In this case format returns #f.

(define format:formatfl-path "formatfl.scm")
;; If floating point formatting is desired, the path for loading the 
;; floating point formatting code is needed. This is not nescessary for
;; SLIB due to the use of the `require' feature.

(define format:iobj-pref "#<")
(define format:iobj-post ">")
;; Internal objects are mostly represented in a #[...] or a #<...> form.

(define (format:force-output port) #t)
;; Flushes an output port.

(define (format:pp obj) (format:error "pretty printing not available"))
;; The pretty printer.

(define format:form-feed (integer->char 12))
;; The form feed character.

(define format:tab (integer->char 9))
;; The tabulator character.

(define format:custom-types #f)
;; Some scheme systems offer more predicates to reveal scheme types than R4RS.
;; To use these predicates for format, you have to define a list of pairs of
;; a predicate and the textual representation. This representation must be a
;; string or it must be a procedure with one argument (which passes the object
;; to represent) returning the textual representation as a string. (see below)
;; If there are no custom types format:custom-types should be defined #f.

(define format:floats (inexact? (string->number "0.0")))
; This detects if the scheme interpreter supports flonums. It can
; be forced to #f if floating point formatting is not desired.

;; Scheme interpreter dependant redefinitions.

(cond

;; SLIB

 ((eq? format:scheme-system 'slib)
  ;; Note: 'pretty-print is not required until ~y is used.
  (set! format:form-feed slib:form-feed)
  (set! format:tab slib:tab)
  (set! format:force-output force-output)
  (provide 'format)
  (if format:floats (require 'format-inexact))
  (set! format:abort (lambda () (slib:error "error in format"))))
 
;; SCM

 ((eq? format:scheme-system 'scm)
  (set! format:force-output force-output)
  (if format:floats
      (load format:formatfl-path))
  (set! format:abort (lambda () (error "error in format"))))

;; ELK

 ((eq? format:scheme-system 'elk)
  ;; Note: cannot use pp (pretty printing) because pp uses format and
  ;;       format is not reentrant
  (set! format:force-output flush-output-port)
  (if format:floats 
      (load format:formatfl-path))
;  (set! (inexact->exact n) n)
  (set! format:iobj-pref "#[")
  (set! format:iobj-post "]")
  (set! format:abort reset)
  (define format:str-port (open-output-string))
  (define (format:iobj->str obj)	; returns the representation of
    (display obj format:str-port)	; internal objects
    (get-output-string format:str-port))
  (set! format:custom-types
	`((,control-point? . ,format:iobj->str)
	  (,procedure?     . ,format:iobj->str)
	  (,output-port?   . ,format:iobj->str)
	  (,input-port?    . ,format:iobj->str)
	  (,environment?   . ,format:iobj->str)
	  )))
 
;; MIT

 ((eq? format:scheme-system 'mit)
  ;(set! format:force-output flush-output)
  (set! format:pp pp)
  (if format:floats 
      (load format:formatfl-path))
  (set! format:iobj-pref "#[")
  (set! format:iobj-post "]")
  (set! format:abort (lambda () (error "error in format")))
  (define (format:iobj->str obj)	; returns the representation of
    (with-output-to-string (lambda () (display obj)))) ; internal objects
  (set! format:custom-types
	`((,continuation? . ,format:iobj->str)
	  (,procedure?    . ,format:iobj->str)
	  (,output-port?  . ,format:iobj->str)
	  (,input-port?   . ,format:iobj->str)
	  (,environment?  . ,format:iobj->str)
	  (,pathname?     . ,format:iobj->str)
	  (,promise?      . ,format:iobj->str)
	  (,record?       . ,format:iobj->str)
	  (,record-type?  . ,format:iobj->str)
	  (,cell?         . ,format:iobj->str)
	  )))

;; Others

 (else
  (if format:floats
      (load format:formatfl-path))
  (define (list-tail l k)		; this isn't a R4RS essential procedure
    (if (zero? k)
	l
	(list-tail (cdr l) (- k 1))))))

;; format:char->string converts a character into a slashified string as
;; done by `write'. The following procedure is dependent on the integer
;; representation of characters and assumes a character number according to
;; the ASCII character set.

(define (format:char->string ch)
  (let ((int-rep (char->integer ch)))
    (string-append "#\\"
      (cond			; THIS IS IMPLEMENTATION DEPENDENT
       ((char=? ch #\newline) "newline")
       ((and (>= int-rep 0) (<= int-rep 32))
	(vector-ref format:ascii-non-printable-charnames int-rep))
       ((= int-rep 127) "del")
       ((>= int-rep 128) (number->string int-rep 8)) ; octal repr.
       (else (string ch))))))

(define format:ascii-non-printable-charnames
  '#("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bel"
     "bs"  "ht"  "nl"  "vt"  "np"  "cr"  "so"  "si"
     "dle" "dc1" "dc2" "dc3" "dc4" "nak" "syn" "etb"
     "can" "em"  "sub" "esc" "fs"  "gs"  "rs"  "us" "space"))

;;; END OF SCHEME IMPLEMENTATION DEPENDENCIES --------------------------------

(define format:version "2.3")
(define format:destination #f)
(define format:output-buffer "")
(define format:flush-output #f)
(define format:case-conversion #f)
(define format:error-continuation #f)
(define format:args #f)
(define format:pos 0)			; curr. format string parsing position
(define format:arg-pos 0)		; curr. format argument position
					; this is global for error presentation

(define (format:out-str str)		; append to format:output-buffer
  (set! format:output-buffer
	(string-append format:output-buffer
		       (if format:case-conversion
			   (format:case-conversion str)
			   str))))

(define (format:out-char ch)
  (format:out-str (string ch)))

(define (format:error . args)		; never returns!
  (let ((error-continuation format:error-continuation)
	(format-args format:args)
	(port (current-error-port)))
    (set! format:error format:intern-error)
    (if (and (>= (length format:args) 2)
	     (string? (cadr format:args)))
	(let ((format-string (cadr format-args)))
	  (if (not (zero? format:arg-pos))
	      (set! format:arg-pos (- format:arg-pos 1)))
	  (format port "FORMAT: error with (format ~a \"~a<===~a\" ~
                              ~{~a ~}===>~{~a ~})~%        "
		  (car format:args)
		  (substring format-string 0 format:pos)
		  (substring format-string format:pos
			     (string-length format-string))
		  (comlist:butlast (cddr format:args) format:arg-pos)
		  (list-tail (cddr format:args) format:arg-pos)))
	(format port "FORMAT: error with (format~{ ~a~})~%        "
		format:args))
    (apply format port args)
    (newline)
    (set! format:error format:error-save)
    (if format:abort			; if format:abort is available
	(format:abort)			; we jump to the top level continuation
	(error-continuation #f))))	; else to the error continuation

(define format:error-save format:error)

(define (format:intern-error . args)   ;if something goes wrong in format:error
  (display "FORMAT: INTERNAL ERROR IN FORMAT:ERROR!") (newline)
  (display "        format args: ") (write format:args) (newline)
  (display "        error args:  ") (write args) (newline)
  (set! format:error format:error-save)
  (if format:abort	
      (format:abort)	
      (format:error-continuation #f)))

(define (format:format-wrapper . args)	; wraps format:format with an error
  (call-with-current-continuation	; continuation
   (lambda (cont)
     (set! format:error-continuation cont)
     (apply format:format args))))

(define (format:format . args)		; the formatter entry
  (set! format:args args)
  (set! format:arg-pos 0)
  (set! format:pos 0)
  (if (< (length args) 2)
      (format:error "not enough arguments"))
  (let ((destination (car args))
	(format-string (cadr args))
	(arglist (cddr args)))
    (set! format:destination
	  (cond
	   ((boolean? destination)
	    (if destination (current-output-port) #f))
	   ((output-port? destination) destination)
	   ((string? destination) destination)
	   (else (format:error "illegal destination `~a'" destination))))
    (if (not (string? format-string))
	(format:error "illegal format string `~a'" format-string))
    (set! format:output-buffer "")
    (set! format:flush-output #f)
    (set! format:case-conversion #f)	; modifier case conversion procedure
    (let ((arg-pos (format:format-work format-string arglist))
	  (arg-len (length arglist)))
      (cond
       ((< arg-pos arg-len)
	(set! format:arg-pos (+ arg-pos 1))
	(set! format:pos (string-length format-string))
	(format:error "~a superfluous argument~:p" (- arg-len arg-pos)))
       ((> arg-pos arg-len)
	(set! format:arg-pos (+ arg-len 1))
	(display format:arg-pos)
	(format:error "~a missing argument~:p" (- arg-pos arg-len)))
       ((output-port? format:destination)
	(display format:output-buffer format:destination)
	(if format:flush-output (format:force-output format:destination))
	#t)
       ((string? format:destination)
	(string-append format:destination format:output-buffer))
       (else format:output-buffer)))))

(define format:parameter-characters
  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+ #\v #\# #\'))

(define (format:format-work format-string arglist) ; does the formatting work
  (letrec
      ((format-string-len (string-length format-string))
       (arg-pos 0)			; argument position in arglist
       (arg-len (length arglist))	; number of arguments
       (modifier #f)			; 'colon | 'at | 'colon-at | #f
       (params '())			; directive parameter list
       (param-value-found #f)		; a directive parameter value found
       (conditional-nest 0)		; conditional nesting level
       (clause-pos 0)			; last cond. clause beginning char pos
       (clause-default #f)		; conditional default clause string
       (clauses '())			; conditional clause string list
       (conditional-type #f)		; reflects the contional modifiers
       (conditional-arg #f)		; argument to apply the conditional
       (iteration-nest 0)		; iteration nesting level
       (iteration-pos 0)		; iteration string beginning char pos
       (iteration-type #f)		; reflects the iteration modifiers
       (max-iterations #f)		; maximum number of iterations
       (recursive-pos-save format:pos)

       (next-char			; gets the next char from format-string
	(lambda ()
	  (let ((ch (peek-next-char)))
	    (set! format:pos (+ 1 format:pos))
	    ch)))

       (peek-next-char
	(lambda ()
	  (if (>= format:pos format-string-len)
	      (format:error "illegal format string")
	      (string-ref format-string format:pos))))

       (one-positive-integer?
	(lambda (params)
	  (cond
	   ((null? params) #f)
	   ((and (integer? (car params))
		 (>= (car params) 0)
		 (= (length params) 1)) #t)
	   (else (format:error "one positive integer parameter expected")))))

       (next-arg
	(lambda ()
	  (if (>= arg-pos arg-len)
	      (begin
		(set! format:arg-pos (+ arg-len 1))
		(format:error "missing argument(s)")))
	  (add-arg-pos 1)
	  (list-ref arglist (- arg-pos 1))))

       (prev-arg
	(lambda ()
	  (add-arg-pos -1)
	  (if (negative? arg-pos)
	      (format:error "missing backward argument(s)"))
	  (list-ref arglist arg-pos)))

       (rest-args
	(lambda ()
	  (let loop ((l arglist) (k arg-pos)) ; list-tail definition
	    (if (zero? k) l (loop (cdr l) (- k 1))))))

       (add-arg-pos
	(lambda (n) 
	  (set! arg-pos (+ n arg-pos))
	  (set! format:arg-pos arg-pos)))

       (anychar-dispatch		; dispatches the format-string
	(lambda ()
	  (if (>= format:pos format-string-len)
	      arg-pos			; used for ~? continuance
	      (let ((char (next-char)))
		(cond
		 ((char=? char #\~)
		  (set! modifier #f)
		  (set! params '())
		  (set! param-value-found #f)
		  (tilde-dispatch))
		 (else
		  (if (and (zero? conditional-nest)
			   (zero? iteration-nest))
		      (format:out-char char))
		  (anychar-dispatch)))))))

       (tilde-dispatch
	(lambda ()
	  (cond
	   ((>= format:pos format-string-len)
	    (format:out-str "~")	; tilde at end of string is just output
	    arg-pos)			; used for ~? continuance
	   ((and (or (zero? conditional-nest)
		     (memv (peek-next-char) ; find conditional directives
			   (append '(#\[ #\] #\; #\: #\@ #\^)
				   format:parameter-characters)))
		 (or (zero? iteration-nest)
		     (memv (peek-next-char) ; find iteration directives
			   (append '(#\{ #\} #\: #\@ #\^)
				   format:parameter-characters))))
	    (case (char-upcase (next-char))

	      ;; format directives

	      ((#\A)			; Any -- for humans
	       (format:out-str
		(format:obj->str-padded (memq modifier '(at colon-at))
					(next-arg)
					(if (eq? modifier 'colon)
					    '(readproof)
					    '())
					params))
	       (anychar-dispatch))
	      ((#\S)			; Slashified -- for parsers
	       (format:out-str
		(format:obj->str-padded (memq modifier '(at colon-at))
					(next-arg)
					(if (eq? modifier 'colon)
					    '(readproof slashify)
					    '(slashify))
					params))
	       (anychar-dispatch))
	      ((#\D)			; Decimal
	       (format:out-str
		(format:num->str-padded modifier (next-arg) params 10 "#d"))
	       (anychar-dispatch))
	      ((#\X)			; Hexadecimal
	       (format:out-str
		(format:num->str-padded modifier (next-arg) params 16 "#x"))
	       (anychar-dispatch))
	      ((#\O)			; Octal
	       (format:out-str
		(format:num->str-padded modifier (next-arg) params 8 "#o"))
	       (anychar-dispatch))
	      ((#\B)			; Binary
	       (format:out-str
		(format:num->str-padded modifier (next-arg) params 2 "#b"))
	       (anychar-dispatch))
	      ((#\R)			; any Radix
	       (if (null? params)
		   (format:error "~~r not implemented")
		   (format:out-str
		    (format:num->str-padded
		     modifier (next-arg) (cdr params) (car params) "#r")))
	       (anychar-dispatch))
	      ((#\F)			; Fixed-format floating-point
	       (format:out-str
		(if format:floats
		    (format:fixed->str modifier (next-arg) params)
		    (number->string (next-arg))))
	       (anychar-dispatch))
	      ((#\E)			; Exponential floating-point
	       (format:out-str
		(if format:floats
		    (format:expon->str modifier (next-arg) params)
		    (number->string (next-arg))))
	       (anychar-dispatch))
	      ((#\G)			; General floating-point
	       (format:out-str
		(if format:floats
		    (format:general->str modifier (next-arg) params)
		    (number->string (next-arg))))
	       (anychar-dispatch))
	      ((#\$)			; Dollars floating-point
	       (format:out-str
		(if format:floats
		    (format:dollar->str modifier (next-arg) params)
		    (number->string (next-arg))))
	       (anychar-dispatch))
	      ((#\C)			; Character
	       (let ((ch (if (one-positive-integer? params)
			     (integer->char (car params))
			     (next-arg))))
		 (if (not (char? ch)) (format:error "~~c expects a character"))
		 (if (eq? modifier 'at)
		     (format:out-str (obj->string ch 'slashify))
		     (format:out-char ch)))
	       (anychar-dispatch))
	      ((#\P)			; Plural
	       (if (memq modifier '(colon colon-at))
		   (prev-arg))
	       (let ((arg (next-arg)))
		 (if (not (number? arg))
		     (format:error "~~p expects a number argument"))
		 (if (= arg 1)
		     (if (memq modifier '(at colon-at))
			 (format:out-str "y"))
		     (if (memq modifier '(at colon-at))
			 (format:out-str "ies")
			 (format:out-str "s"))))
	       (anychar-dispatch))
	      ((#\~)		; Tilde
	       (if (one-positive-integer? params)
		   (format:out-str (make-string (car params) #\~))
		   (format:out-str "~"))
	       (anychar-dispatch))
	      ((#\% #\&)		; Newline (Freshline is the same)
	       (if (one-positive-integer? params)
		   (format:out-str (make-string (car params) #\newline))
		   (format:out-char #\newline))
	       (anychar-dispatch))
	      ((#\_)			; Space
	       (if (one-positive-integer? params)
		   (format:out-str (make-string (car params) #\space))
		   (format:out-str " "))
	       (anychar-dispatch))
	      ((#\T)			; Tab
	       (if (one-positive-integer? params)
		   (format:out-str (make-string (car params) format:tab))
		   (format:out-char format:tab))
	       (anychar-dispatch))
	      ((#\|)			; Page Seperator
	       (if (one-positive-integer? params)
		   (format:out-str (make-string (car params) format:form-feed))
		   (format:out-char format:form-feed))
	       (anychar-dispatch))
	      ((#\Y)			; Pretty-print
	       (case format:scheme-system 
		 ((slib)
		  (require 'pretty-print)
		  (set! format:pp pretty-print)))
	       (if (not format:destination)
		   (format:error "~~y not supported with string output")
		   (format:pp (next-arg)))
	       (anychar-dispatch))
	      ((#\? #\K)		; Indirection (is "~K" in T)
	       (cond
		((memq modifier '(colon colon-at))
		 (format:error "illegal modifier in ~~?"))
		((eq? modifier 'at)
		 (let* ((frmt (next-arg))
			(args (rest-args)))
		   (add-arg-pos (format:format-work frmt args))))
		(else
		 (let* ((frmt (next-arg))
			(args (next-arg)))
		   (format:format-work frmt args))))
	       (anychar-dispatch))
	      ((#\!)			; Flush output
	       (set! format:flush-output #t)
	       (anychar-dispatch))
	      ((#\newline)		; Continuation lines
	       (if (eq? modifier 'at)
		   (format:out-char #\newline))
	       (if (< format:pos format-string-len)
		   (do ((ch (peek-next-char) (peek-next-char)))
		       ((or (not (char-whitespace? ch))
			    (= format:pos (- format-string-len 1))))
		     (if (eq? modifier 'colon)
			 (format:out-char (next-char))
			 (next-char))))
	       (anychar-dispatch))
	      ((#\*)			; Argument jumping
	       (case modifier
		 ((colon)		; jump backwards
		  (if (one-positive-integer? params)
		      (do ((i 0 (+ i 1)))
			  ((= i (car params)))
			(prev-arg))
		      (prev-arg)))
		 ((at)			; jump absolute
		  (set! arg-pos (if (one-positive-integer? params)
				    (car params) 0)))
		 ((colon-at)
		  (format:error "illegal modifier `:@' in ~~* directive"))
		 (else			; jump forward
		  (if (one-positive-integer? params)
		      (do ((i 0 (+ i 1)))
			  ((= i (car params)))
			(next-arg))
		      (next-arg))))
	       (anychar-dispatch))
	      ((#\()			; Case conversion begin
	       (set! format:case-conversion
		     (case modifier
		       ((at) string-capitalize-first)
		       ((colon) string-capitalize)
		       ((colon-at) string-upcase)
		       (else string-downcase)))
	       (anychar-dispatch))
	      ((#\))			; Case conversion end
	       (if (not format:case-conversion)
		   (format:error "missing ~~("))
	       (set! format:case-conversion #f)
	       (anychar-dispatch))
	      ((#\[)			; Conditional begin
	       (set! conditional-nest (+ conditional-nest 1))
	       (cond
		((= conditional-nest 1)
		 (set! clause-pos format:pos)
		 (set! clause-default #f)
		 (set! clauses '())
		 (set! conditional-type
		       (case modifier
			 ((at) 'if-then)
			 ((colon) 'if-else-then)
			 ((colon-at) (format:error "illegal modifier in ~~["))
			 (else 'num-case)))
		 (set! conditional-arg
		       (if (one-positive-integer? params)
			   (car params)
			   (next-arg)))))
	       (anychar-dispatch))
	      ((#\;)                    ; Conditional separator
	       (if (zero? conditional-nest)
		   (format:error "~~; not in ~~[~~] conditional"))
	       (if (not (null? params))
		   (format:error "no parameter allowed in ~~;"))
	       (if (= conditional-nest 1)
		   (let ((clause-str
			  (cond
			   ((eq? modifier 'colon)
			    (set! clause-default #t)
			    (substring format-string clause-pos 
				       (- format:pos 3)))
			   ((memq modifier '(at colon-at))
			    (format:error "illegal modifier in ~~;"))
			   (else
			    (substring format-string clause-pos
				       (- format:pos 2))))))
		     (set! clauses (append clauses (list clause-str)))
		     (set! clause-pos format:pos)))
	       (anychar-dispatch))
	      ((#\])			; Conditional end
	       (if (zero? conditional-nest) (format:error "missing ~~["))
	       (set! conditional-nest (- conditional-nest 1))
	       (if modifier
		   (format:error "no modifier allowed in ~~]"))
	       (if (not (null? params))
		   (format:error "no parameter allowed in ~~]"))
	       (cond
		((zero? conditional-nest)
		 (let ((clause-str (substring format-string clause-pos
					      (- format:pos 2))))
		   (if clause-default
		       (set! clause-default clause-str)
		       (set! clauses (append clauses (list clause-str)))))
		 (case conditional-type
		   ((if-then)
		    (if conditional-arg
			(format:format-work (car clauses)
					    (list conditional-arg))))
		   ((if-else-then)
		    (add-arg-pos
		     (format:format-work (if conditional-arg
					     (cadr clauses)
					     (car clauses))
					 (rest-args))))
		   ((num-case)
		    (if (or (not (integer? conditional-arg))
			    (< conditional-arg 0))
			(format:error "argument not a positive integer"))
		    (if (not (and (>= conditional-arg (length clauses))
				  (not clause-default)))
			(add-arg-pos
			 (format:format-work
			  (if (>= conditional-arg (length clauses))
			      clause-default
			      (list-ref clauses conditional-arg))
			  (rest-args))))))))
	       (anychar-dispatch))
	      ((#\{)			; Iteration begin
	       (set! iteration-nest (+ iteration-nest 1))
	       (cond
		((= iteration-nest 1)
		 (set! iteration-pos format:pos)
		 (set! iteration-type
		       (case modifier
			 ((at) 'rest-args)
			 ((colon) 'sublists)
			 ((colon-at) 'rest-sublists)
			 (else 'list)))
		 (set! max-iterations (if (one-positive-integer? params)
					 (car params) #f))))
	       (anychar-dispatch))
	      ((#\})			; Iteration end
	       (if (zero? iteration-nest) (format:error "missing ~~{"))
	       (set! iteration-nest (- iteration-nest 1))
	       (case modifier
		 ((colon)
		  (if (not max-iterations) (set! max-iterations 1)))
		 ((colon-at at) (format:error "illegal modifier"))
		 (else (if (not max-iterations) (set! max-iterations 100))))
	       (if (not (null? params))
		   (format:error "no parameters allowed in ~~}"))
	       (if (zero? iteration-nest)
		 (let ((iteration-str
			(substring format-string iteration-pos
				   (- format:pos (if modifier 3 2)))))
		   (if (string=? iteration-str "")
		       (set! iteration-str (next-arg)))
		   (case iteration-type
		     ((list)
		      (let ((args (next-arg))
			    (args-len 0))
			(if (not (list? args))
			    (format:error "expected a list argument"))
			(set! args-len (length args))
			(do ((arg-pos 0 (+ arg-pos
					   (format:format-work
					    iteration-str
					    (list-tail args arg-pos))))
			     (i 0 (+ i 1)))
			    ((or (>= arg-pos args-len)
				 (>= i max-iterations))))))
		     ((sublists)
		      (let ((args (next-arg))
			    (args-len 0))
			(if (not (list? args))
			    (format:error "expected a list argument"))
			(set! args-len (length args))
			(do ((arg-pos 0 (+ arg-pos 1)))
			    ((or (>= arg-pos args-len)
				 (>= arg-pos max-iterations)))
			  (let ((sublist (list-ref args arg-pos)))
			    (if (not (list? sublist))
				(format:error
				 "expected a list of lists argument"))
			    (format:format-work iteration-str sublist)))))
		     ((rest-args)
		      (let* ((args (rest-args))
			     (args-len (length args))
			     (usedup-args
			      (do ((arg-pos 0 (+ arg-pos
						 (format:format-work
						  iteration-str
						  (list-tail args arg-pos))))
				   (i 0 (+ i 1)))
				  ((or (>= arg-pos args-len)
				       (>= i max-iterations))
				   arg-pos))))
			(add-arg-pos usedup-args)))
		     ((rest-sublists)
		      (let* ((args (rest-args))
			     (args-len (length args))
			     (usedup-args
			      (do ((arg-pos 0 (+ arg-pos 1)))
				  ((or (>= arg-pos args-len)
				       (>= arg-pos max-iterations))
				   arg-pos)
				(let ((sublist (list-ref args arg-pos)))
				  (if (not (list? sublist))
				      (format:error "expected list arguments"))
				  (format:format-work iteration-str sublist)))))
			(add-arg-pos usedup-args)))
		     (else (format:error "internal error in ~~}")))))
	       (anychar-dispatch))
	      ((#\^)			; Up and out
	       (let* ((continue
		       (cond
			((not (null? params))
			 (not
			  (case (length params)
			   ((1) (zero? (car params)))
			   ((2) (= (list-ref params 0) (list-ref params 1)))
			   ((3) (<= (list-ref params 0)
				    (list-ref params 1)
				    (list-ref params 2)))
			   (else (format:error "too much parameters")))))
			(format:case-conversion ; if conversion stop conversion
			 (set! format:case-conversion string-copy) #t)
			((= iteration-nest 1) #t)
			((= conditional-nest 1) #t)
			((>= arg-pos arg-len)
			 (set! format:pos format-string-len) #f)
			(else #t))))
		 (if continue
		     (anychar-dispatch))))

	      ;; format directive modifiers and parameters

	      ((#\@)			; `@' modifier
	       (if (eq? modifier 'colon-at)
		   (format:error "double `@' modifier"))
	       (set! modifier (if (eq? modifier 'colon) 'colon-at 'at))
	       (tilde-dispatch))
	      ((#\:)			; `:' modifier
	       (if modifier (format:error "illegal `:' modifier position"))
	       (set! modifier 'colon)
	       (tilde-dispatch))
	      ((#\')			; Character parameter
	       (if modifier (format:error "misplaced modifier"))
	       (set! params (append params (list (char->integer (next-char)))))
	       (set! param-value-found #t)
	       (tilde-dispatch))
	      ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\+) ; num. paramtr
	       (if modifier (format:error "misplaced modifier"))
	       (let ((num-str-beg (- format:pos 1))
		     (num-str-end format:pos))
		 (do ((ch (peek-next-char) (peek-next-char)))
		     ((not (char-numeric? ch)))
		   (next-char)
		   (set! num-str-end (+ 1 num-str-end)))
		 (set! params
		       (append params
			       (list (string->number
				      (substring format-string
						 num-str-beg
						 num-str-end))))))
	       (set! param-value-found #t)
	       (tilde-dispatch))
	      ((#\V)			; Variable parameter from next argum.
	       (if modifier (format:error "misplaced modifier"))
	       (set! params (append params (list (next-arg))))
	       (set! param-value-found #t)
	       (tilde-dispatch))
	      ((#\#)			; Parameter is number of remaining args
	       (if modifier (format:error "misplaced modifier"))
	       (set! params (append params (list (length (rest-args)))))
	       (set! param-value-found #t)
	       (tilde-dispatch))
	      ((#\,)			; Parameter separators
	       (if modifier (format:error "misplaced modifier"))
	       (if (not param-value-found)
		   (set! params (append params '(#f)))) ; append empty paramtr
	       (set! param-value-found #f)
	       (tilde-dispatch))
	      (else			; Unknown tilde directive
	       (format:error "unknown control character `~c'"
		      (string-ref format-string (- format:pos 1))))))
	   (else (anychar-dispatch)))))) ; in case of conditional

    (set! format:pos 0)
    (set! format:arg-pos 0)
    (anychar-dispatch)			; start the formatting
    (set! format:pos recursive-pos-save)
    arg-pos))				; return the position in the arg. list


(define (format:obj->str-padded pad-left obj options params)
  (let ((mincol 0)
	(colinc 1)			; sorry I don't understand this CL parm
	(minpad 0)
	(padchar #\space)
	(objstr (apply obj->string (append (list obj) options))))
    (if (null? params)
	objstr
	(begin
	  (set! params (append params '(#f #f #f #f)))
	  (if (list-ref params 0) (set! mincol (list-ref params 0)))
	  (if (list-ref params 1) (set! colinc (list-ref params 1)))
	  (if (list-ref params 2) (set! minpad (list-ref params 2)))
	  (if (list-ref params 3)
	      (set! padchar (integer->char (list-ref params 3))))
	  (format:pad-str objstr (negative? mincol) pad-left
			  (abs mincol) minpad padchar)))))


(define (format:num->str-padded modifier number params radix-num radix-prefix)
  (if (not (number? number)) (format:error "argument not a number"))
  (let ((mincol 0)
	(padchar #\space)
	(commachar #\,)
	(commawidth 3)			; an extension to CL
	(numstr-len 0)
	(numstr (number->string number radix-num)))

    (if (and (null? params) (not modifier))
	numstr
	(begin
	  (set! params (append params '(#f #f #f #f)))
	  (if (list-ref params 0) (set! mincol (list-ref params 0)))
	  (if (list-ref params 1)
	      (set! padchar (integer->char (list-ref params 1))))
	  (if (list-ref params 2)
	      (set! commachar (integer->char (list-ref params 2))))
	  (if (list-ref params 3) (set! commawidth (list-ref params 3)))
	  (set! numstr-len (string-length numstr))

	  (if (and (memq modifier '(colon colon-at)) ; insert comma character
		   (integer? number))	; only integers are ,-separated
	      (set! numstr
		    (do ((s "")
			 (i (- numstr-len commawidth) (- i commawidth)))
			((or (zero? i) (negative? i))
			 (string-append
			  (substring numstr 0 (+ i commawidth )) s))
		      (set! s (string-append
			       (string commachar)
			       (substring numstr i (+ i commawidth)) s)))))

	  (if (memq modifier '(at colon-at))	; append numerical prefix
	      (set! numstr (string-append radix-prefix numstr)))

	  (format:pad-str numstr (negative? mincol) #t
			  (abs mincol) 0 padchar)))))

(define (format:pad-str objstr fixed-field pad-left mincol minpad padchar)
  (let ((objstr-len (string-length objstr)))
    (if fixed-field
	(if (> objstr-len mincol)
	    (if pad-left
		(string-append "<"
		 (substring objstr (- objstr-len (- mincol 1)) objstr-len))
		(string-append (substring objstr 0 (- mincol 1)) ">"))
	    (if pad-left
		(string-append (make-string (- mincol objstr-len) padchar)
			       objstr)
		(string-append objstr
			       (make-string (- mincol objstr-len) padchar))))
	(if (> objstr-len mincol)
	    (if pad-left
		(string-append (make-string minpad padchar) objstr)
		(string-append objstr (make-string minpad padchar)))
	    (if pad-left
		(string-append (make-string (- mincol objstr-len) padchar)
			       objstr)
		(string-append objstr
			       (make-string (- mincol objstr-len) padchar)))))
    ))

;; obj->string converts an arbitrary scheme object to a string.
;; `options' is a list of options which may contain the following symbols:
;;   slashify:      slashifies output string as (write) does
;;   readproof:     prints out internal objects as quoted strings so
;;                  that the output can always be processed by (read)
;; obj->string imports format:char->string which converts a character into
;; a slashified string as `write' does and which is implementation dependent.

(define (obj->string obj . options)
  (let to-str ((obj obj)
	       (slashify (if (memq 'slashify options) #t #f))
	       (readproof (if (memq 'readproof options) #t #f)))
    (cond
     ((and format:custom-types
	   (format:custom-type? obj format:custom-types))
      (if readproof
	  (string-append "\"" format:custom-str "\"")
	  format:custom-str))
     ((string? obj)
      (if slashify
	  (let ((obj-len (string-length obj)))
	    (string-append
	     "\""
	     (let loop ((i 0) (j 0))	; taken from Marc Feeley's pp.scm
	       (if (= j obj-len)
		   (string-append (substring obj i j) "\"")
		   (let ((c (string-ref obj j)))
		     (if (or (char=? c #\\)
			     (char=? c #\"))
			 (string-append (substring obj i j) "\\"
					(loop j (+ j 1)))
			 (loop i (+ j 1))))))))
	  obj))

     ((boolean? obj) (if obj "#t" "#f"))

     ((number? obj) (number->string obj))

     ((symbol? obj) (symbol->string obj))

     ((char? obj)
      (if slashify
	  (format:char->string obj)
	  (string obj)))

     ((null? obj) "()")

     ((procedure? obj) (iobj-str "procedure" readproof))

     ((output-port? obj) (iobj-str "output-port" readproof))

     ((input-port? obj) (iobj-str "input-port" readproof))

     ((list? obj)
      (string-append "("
		     (let loop ((obj-list obj))
		       (if (null? (cdr obj-list))
			   (to-str (car obj-list) 'slashify readproof)
			   (string-append
			    (to-str (car obj-list) 'slashify readproof)
			    " "
			    (loop (cdr obj-list)))))
		     ")"))

     ((pair? obj)
      (string-append "("
		     (to-str (car obj) 'slashify readproof)
		     " . "
		     (to-str (cdr obj) 'slashify readproof)
		     ")"))

     ((eof-object? obj) (iobj-str "eof-object" readproof))

     ((vector? obj)
      (string-append "#" (to-str (vector->list obj) 'slashify readproof)))

     (else (iobj-str "unspecified" readproof))
    ))
)

;; format:custom-type? detects if `obj' has a custom type and sets
;; format:custom-str to its representation according to format:custom-types

(define (format:custom-type? obj pred-l) 
  (cond
   ((null? pred-l) #f)
   (((caar pred-l) obj)
    (let ((rep (cdar pred-l)))
      (set! format:custom-str (if (procedure? rep) (rep obj) rep))))
   (else (format:custom-type? obj (cdr pred-l)))))

(define format:custom-str "")

;; iobj-str makes an internal object string

(define (iobj-str obj-str readproof)
  (if readproof
      (string-append "\"" format:iobj-pref obj-str format:iobj-post "\"")
      (string-append format:iobj-pref obj-str format:iobj-post)))

(require 'common-list-functions)
(require 'string-case)
;; string-upcase, string-downcase, string-capitalize, string-capitalize-first
;; are obvious string conversion procedures and are non destructive.

(define (string-capitalize-first str)	; "hello" -> "Hello"
  (let ((cap-str (string-copy str))	; "hELLO" -> "Hello"
	(non-first-alpha #f)		; "*hello" -> "*Hello"
	(str-len (string-length str)))	; "hello you" -> "Hello you"
    (do ((i 0 (+ i 1)))
	((= i str-len) cap-str)
      (let ((c (string-ref str i)))
	(if (char-alphabetic? c)
	    (if non-first-alpha
		(string-set! cap-str i (char-downcase c))
		(begin
		  (set! non-first-alpha #t)
		  (string-set! cap-str i (char-upcase c)))))))))

;; string-index finds the index of the first occurence of the character `c'
;; in the string `s'; it returns #f if there is no such character in `s'.

(define (string-index s c)
  (let ((slen-1 (- (string-length s) 1)))
    (let loop ((i 0))
      (cond
       ((char=? c (string-ref s i)) i)
       ((= i slen-1) #f)
       (else (loop (+ i 1)))))))

(define format (if format:abort
		   format:format	   ; without error continuation
		   format:format-wrapper)) ; with    error continuation


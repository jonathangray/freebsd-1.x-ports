;; formatfl.scm (required by format.scm version 2.3)

(define format:space-ch (char->integer #\space))

;; format fixed flonums (~F)

(define (format:fixed->str modifier number params)
  (if (not (number? number))
      (format:error "argument not a number"))

  (let* ((pl (length params))
	 (width (if (> pl 0) 
		    (let ((a (list-ref params 0)))
		      (if (and a (< a 0)) 
			  (format:error
			   "width parameter must be a positive integer"))
		      a)
		    #f))
	 (digits (if (> pl 1) 
		     (let ((a (list-ref params 1)))
		       (if (and a (< a 0))
			   (format:error
			    "digits parameter must be a positive integer"))
		       a)
		     #f))
	 (scale (if (> pl 2)
		    (let ((a (list-ref params 2)))
		      (if a
			  (set! number (* number (expt 10 a))))
		      a)
		    #f))
	 (overch (if (> pl 3)
		     (list-ref params 3) #f))
	 (padch (if (> pl 4) 
		    (let ((a (list-ref params 4)))
		      (if a a format:space-ch))
		    format:space-ch)))

    (if digits
	(let* ((fl (format:parse-float number 'fixed 0)) ; fixed precision
	       (frc-str (cadr fl))
	       (dot (caddr fl))
	       (frc-len (cadddr fl))
	       (frc (if (<= (- frc-len dot) digits)
			(string-append frc-str
				       (make-string (- digits (- frc-len dot))
						    #\0))
			(let ((r (format:round-numstr frc-str (+ dot digits))))
			  (set! dot (+ dot (cadr r)))
			  (car r))))
	       (numstr
		(string-append 
		 (if (car fl) (if (eq? modifier 'at) "+" "") "-")
		 (if (zero? dot)
		     (if (and width (= width (+ digits 1))) "" "0")
		     (substring frc 0 dot))
		 "."
		 (substring frc dot (string-length frc))))
	       (numlen (string-length numstr)))
	  (cond
	   ((not width)
	    numstr)
	   ((< numlen width)
	    (string-append
	     (make-string (- width numlen) (integer->char padch)) numstr))
	   ((= numlen width)
	    numstr)
	   (else
	    (if overch
		(make-string width (integer->char overch))
		numstr))))
	
	(let* ((fl (format:parse-float number 'fixed 0)) ; else free precision
	       (dot (caddr fl))
	       (frc-len (cadddr fl))
	       (frc (format:strip-trailing-zeros (cadr fl) dot frc-len))
	       (numstr
		(string-append 
		 (if (car fl) (if (eq? modifier 'at) "+" "") "-")
		 (if (zero? dot) "0" (substring frc 0 dot))
		 "."
		 (substring frc dot (string-length frc)))))
	  (if width
	      (let ((numlen (string-length numstr)))
		(cond			; with fixed width
		 ((= numlen width)
		  numstr)
		 ((< numlen width)
		  (string-append
		   (make-string (- width numlen) (integer->char padch))
		   numstr))
		 (else
		  (let ((index (string-index numstr #\.)))
		    (if (<= width index)
			(if overch	; numstr too big for required width
			    (make-string width (integer->char overch))
			    numstr)
			(format:fixed->str modifier number ; adjust precision
					   (list width (- (- width index) 1)
						 scale overch padch)))))))
	      numstr)))))

;; format exponential flonums (~E)

(define (format:expon->str modifier number params)
  (if (not (number? number))
      (format:error "argument not a number"))
    
  (let* ((pl (length params))
	 (width (if (> pl 0) 
		    (let ((a (list-ref params 0)))
		      (if (and a (< a 0)) 
			  (format:error
			   "width parameter must be a positive integer"))
		      a)
		    #f))
	 (digits (if (> pl 1) 
		     (let ((a (list-ref params 1)))
		       (if (and a (< a 0))
			   (format:error
			    "digits parameter must be a positive integer"))
		       a)
		     #f))
	 (edigits (if (> pl 2) 
		      (let ((a (list-ref params 2)))
			(if (and a (< a 0))
			    (format:error
			     "exponent digits parameter must be a positive integer"))
			a)
		      #f))
	 (scale (if (> pl 3)
		    (let ((a (list-ref params 3)))
		      (if a a 1))
		    1))
	 (overch (if (> pl 4)
		     (list-ref params 4) #f))
	 (padch (if (> pl 5) 
		    (let ((a (list-ref params 5)))
		      (if a a format:space-ch))
		    format:space-ch))
	 (expch (if (> pl 6)
		    (list-ref params 6) #f)))
	 
    (if digits
	(let* ((fl (format:parse-float number 'expon scale)) ; fixed precision
	       (digits (if (> scale 0)
			   (if (< scale (+ digits 2))
			       (+ (- digits scale) 1)
			       0)
			   digits))
	       (frc-str (list-ref fl 1))
	       (dot (list-ref fl 2))
	       (frc-len (list-ref fl 3))
	       (frc (if (<= (- frc-len dot) digits)
			(string-append frc-str
				       (make-string (- digits (- frc-len dot))
						    #\0))
			(let ((r (format:round-numstr frc-str (+ dot digits))))
			  (set! dot (+ dot (cadr r)))
			  (car r))))
	       (exp-str (list-ref fl 5))
	       (numstr
		(string-append 
		 (if (car fl) (if (eq? modifier 'at) "+" "") "-")
		 (if (zero? dot) "0" (substring frc 0 dot))
		 "."
		 (substring frc dot (string-length frc))
		 (if expch (string (integer->char expch)) "E")
		 (if (list-ref fl 4) "+" "-")
		 (if edigits
		     (let ((exp-len (string-length exp-str)))
		       (cond 
			((< exp-len edigits)
			 (string-append
			  (make-string (- edigits exp-len) #\0) exp-str))
			((and (> exp-len edigits) width overch)
			 "?")		; E-digits overflow
			(else exp-str)))
		     exp-str)))
	       (numlen (string-length numstr)))
	  (if (and (zero? dot) width (= width (- numlen 1)))
	      (let ((dotpos (string-index numstr #\.))) ; skip leading zero
		(set! numstr (string-append
			      (substring numstr 0 (- dotpos 1))
			      (substring numstr dotpos numlen)))
		(set! numlen (- numlen 1))))
	  (cond
	   ((not width)
	    numstr)
	   ((char=? (string-ref numstr (- numlen 1)) #\?) ; E-digits overflow
	    (make-string width (integer->char overch)))
	   ((< numlen width)
	    (string-append
	     (make-string (- width numlen) (integer->char padch)) numstr))
	   ((= numlen width)
	    numstr)
	   (else
	    (if overch
		(make-string width (integer->char overch))
		numstr))))
	
	(let* ((fl (format:parse-float number 'expon scale)) ; free precision
	       (dot (list-ref fl 2))
	       (frc-len (list-ref fl 3))
	       (frc (format:strip-trailing-zeros (list-ref fl 1) dot frc-len))
	       (exp-str (list-ref fl 5))
	       (numstr
		(string-append 
		 (if (car fl) (if (eq? modifier 'at) "+" "") "-")
		 (if (zero? dot) "0" (substring frc 0 dot))
		 "."
		 (substring frc dot (string-length frc))
		 (if expch (string (integer->char expch)) "E")
		 (if (list-ref fl 4) "+" "-")
		 (if edigits
		     (let ((exp-len (string-length exp-str)))
		       (cond 
			((< exp-len edigits)
			 (string-append
			  (make-string (- edigits exp-len) #\0) exp-str))
			((and (> exp-len edigits) width overch)
			 "?")		; E-digits overflow
			(else exp-str)))
		     exp-str))))
	  (if width
	      (let ((numlen (string-length numstr)))
		(cond			; with fixed width
		 ((char=? (string-ref numstr (- numlen 1)) #\?) ; E-digits ovfl
		  (make-string width (integer->char overch)))
		 ((= numlen width)
		  numstr)
		 ((and (= numlen (+ width 1)) (zero? dot))
		  (let ((dotpos (string-index numstr #\.))) ; skip leading zero
		    (string-append
		     (substring numstr 0 (- dotpos 1))
		     (substring numstr dotpos numlen))))
		 ((< numlen width)
		  (string-append
		   (make-string (- width numlen) (integer->char padch))
		   numstr))
		 (else
		  (let* ((dotpos (string-index numstr #\.))
			 (exppos (string-index numstr (if expch
							  (integer->char expch)
							  #\E)))
			 (notdigits (+ dotpos (- numlen exppos) 1)))
		    (if (<= width notdigits)
			(if overch	; numstr too big for required width
			    (make-string width (integer->char overch))
			    numstr)
			(format:expon->str ; adjust digits
			 modifier number (list width (- width notdigits)
					       edigits scale
					       overch padch expch)))))))
	      numstr)))))

;; format general flonums (~G)

(define format:log10 (log 10))

(define (format:general->str modifier number params)
  (let* ((exp-numstr (format:expon->str modifier number params))
	 (pl (length params))
	 (width (if (> pl 0) (list-ref params 0) #f))
	 (digits (if (> pl 1) (list-ref params 1) #f))
	 (edigits (if (> pl 2) (list-ref params 2) #f))
	 (overch (if (> pl 4) (list-ref params 4) #f))
	 (padch (if (> pl 5) (list-ref params 5) #f))
	 (expch (if (> pl 6) (integer->char (list-ref params 6)) #\E))
	 (ee (if edigits (+ edigits 2) 4))
	 (ww (if width (- width ee) #f))
	 (n (if (zero? number)
		0
		(let ((n (/ (log (abs number)) format:log10)))
		  (if (> n 0)
		      (set! n (+ n 1)))
		  (inexact->exact (truncate n)))))
	 (d (if digits
		digits
		(let ((q (- (string-index exp-numstr expch)
			    (string-index exp-numstr #\.) 1)))
		  (max q (min n 7)))))
	 (dd (- d n)))
    (if (<= 0 dd d)
	(string-append
	 (format:fixed->str modifier number (list ww dd #f overch padch))
	 (make-string ee #\space))
	exp-numstr)))

;; format dollar flonums (~$)

(define (format:dollar->str modifier number params)
  (if (not (number? number))
      (format:error "argument not a number"))

  (let* ((pl (length params))
	 (digits (if (> pl 0) 
		     (let ((a (list-ref params 0)))
		       (if a
			   (if (< a 0)
			       (format:error
				"digits parameter must be a positive integer")
			       a)
			   2))
		     2))
	 (scale (if (> pl 1)
		    (let ((a (list-ref params 1)))
		      (if a
			  (if (< a 0)
			      (format:error
			       "scale parameter must be a positive integer")
			      a)
			  1))
		    1))
	 (width (if (> pl 2) 
		    (let ((a (list-ref params 2)))
		      (if a
			  (if (< a 0)
			      (format:error
			       "width parameter must be a positive integer")
			      a)
			  0))
		    0))
	 (padch (if (> pl 3) 
		    (let ((a (list-ref params 3)))
		      (if a a format:space-ch))
		    format:space-ch)))
    
    (let* ((fl (format:parse-float number 'fixed 0))
	   (frc-str (list-ref fl 1))
	   (dot (list-ref fl 2))
	   (frc-len (list-ref fl 3))
	   (frc (if (<= (- frc-len dot) digits)
		    (string-append frc-str
				   (make-string (- digits (- frc-len dot))
						#\0))
		    (let ((r (format:round-numstr frc-str (+ dot digits))))
		      (set! dot (+ dot (cadr r)))
		      (car r))))
	   (numstr
	    (string-append
	     (if (and scale (> scale dot)) (make-string (- scale dot) #\0) "")
	     (if (zero? dot) "0" (substring frc 0 dot))
	     "."
	     (substring frc dot (string-length frc))))
	   (numlen (string-length numstr)))
      (string-append
       (if (< numlen width)
	   (case modifier
	     ((colon)
	      (string-append
	       (if (car fl) "" "-")
	       (make-string (- width numlen (if (car fl) 0 1))
			    (integer->char padch))))
	     ((at)
	      (string-append
	       (make-string (- width numlen 1) (integer->char padch))
	       (if (car fl) "+" "-")))
	     ((colon-at)
	      (string-append
	       (if (car fl) "+" "-")
	       (make-string (- width numlen 1) (integer->char padch))))
	     (else
	      (string-append
	       (make-string (- width numlen (if (car fl) 0 1))
			    (integer->char padch))
	       (if (car fl) "" "-"))))
	   (if (car fl)
	       (if (memq modifier '(at colon-at)) "+" "")
	       "-"))
       numstr))))


;; Parse the output of number->string of a flonum and return a list of
;; parameters specified in this order:
;; 0. BOOL number >=0?
;; 1. STRING number fraction
;; 2. INTEGER dot position in the fraction
;; 3. INTEGER number fraction string length 
;; 4. BOOL exponent >=0?      (only if (eq? format 'expon))
;; 5. STRING exponent number  (only if (eq? format 'expon))
;;

(define (format:parse-float n format scale)
  (let* ((numstr (number->string n))
	 (numlen (string-length numstr))
	 (state 'beg)
	 (frc-beg #f)			; fractional part
	 (frc-str "")
	 (frc-len 0)			; length of the fraction string
	 (frc-sgn #t)			; sign: #t: positive #f: negative
	 (exp-beg #f)			; exponential part
	 (exp-str "0")
	 (exp-sgn #t)			; sign: #t: positive #f: negative
	 (dot-pos #f)
	 (zeros? #t)
	 (zeros 0))
    (do ((i 0 (+ i 1)))
	((= i numlen)
	 (case state 
	   ((dig)
	    (if exp-beg
		(set! exp-str (substring numstr exp-beg i))
		(begin
		  (set! frc-str (string-append frc-str 
					       (substring numstr frc-beg i)))
		  (set! frc-len (+ frc-len (- i frc-beg))))))
	   ((beg)			
	    (if zeros?
		(begin			; we found a real zero 
		  (set! frc-str "0")
		  (set! frc-len 1)
		  (set! dot-pos 1)
		  (set! zeros 0))))	; else we have a "nnn."
	   (else
	    (format:error
	     "unknown floating point representation in number->string")))
	 (if (not dot-pos)
	     (set! dot-pos (- i frc-beg))))
      (let ((c (string-ref numstr i)))	; parse the output of number->string
	(cond				; which can be any valid number
	 ((char-numeric? c)		; representation of R4RS, except 
	  (case state			; complex, and return a form like
	    ((beg) (if (and zeros? (char=? c #\0))
		       (set! zeros (+ zeros 1))
		       (begin
			 (set! frc-beg i)
			 (set! state 'dig))))
	    ((exp) (set! exp-beg i)	; and exponent string
		   (set! state 'dig)))
	  (if (and zeros? (not (char=? c #\0)))
	      (set! zeros? #f)))
	 ((or (char=? c #\-) (char=? c #\+))
	  (case state
	    ((beg) (set! frc-sgn (char=? c #\+)))
	    ((exp) (set! exp-beg i)
		   (set! state 'dig))))
	 ((char=? c #\.)
	  (if zeros?
	      (set! dot-pos zeros)
	      (begin
		(set! frc-str (substring numstr frc-beg i))
		(set! frc-len (- i frc-beg))
		(set! dot-pos frc-len)))
	  (set! state 'beg))
	 ((or (char=? c #\e) (char=? c #\E))
	  (set! frc-str (string-append frc-str (substring numstr frc-beg i)))
	  (set! frc-len (+ frc-len (- i frc-beg)))
	  (if (not dot-pos) (set! dot-pos (string-length frc-str)))
	  (set! state 'exp))
	 ((char-whitespace? c) #t)
	 ((char=? c #\d) #t)		; decimal radix prefix
	 ((char=? c #\#) #t)
	 (else
	  (format:error "illegal character `~c' in number->string" c)))))
    
    ;; now format the parsed values according to format's need

    (case format
      ((expon)				; exponential format
       (let ((exp (- (+ (- dot-pos scale) (string->number exp-str)) zeros)))
	 (set! exp-sgn (>= exp 0))
	 (set! exp-str (number->string exp))
	 (if (or (char=? (string-ref exp-str 0) #\-)
		 (char=? (string-ref exp-str 0) #\+))
	     (set! exp-str (substring exp-str 1 (string-length exp-str))))
	 (cond 
	  ((< scale 0)		; leading zero
	   (set! frc-str (string-append
			  (make-string (- scale) #\0) frc-str))
	   (set! frc-len (+ frc-len (- scale)))
	   (set! scale 0))
	  ((> scale dot-pos)
	   (set! frc-str (string-append 
			  frc-str 
			  (make-string (- scale dot-pos) #\0)))
	   (set! frc-len (+ frc-len (- scale dot-pos)))))
	 (list frc-sgn frc-str scale frc-len exp-sgn exp-str)))
      ((fixed)				; fixed format
       (let ((exp (- (string->number exp-str) zeros))
	     (old-dot dot-pos))
	 (set! dot-pos (+ dot-pos exp))
	 (cond
	  ((< dot-pos 0) 
	   (set! frc-str (string-append
			  (make-string (- (- exp) old-dot) #\0)
			  frc-str))
	   (set! frc-len (+ frc-len (- (- exp) old-dot)))
	   (set! dot-pos 0))
	  ((> dot-pos frc-len)
	   (set! frc-str (string-append
			  frc-str
			  (make-string (- exp (- frc-len old-dot)) #\0)))
	   (set! frc-len (+ frc-len (- exp (- frc-len old-dot)))))))
       (list frc-sgn frc-str dot-pos frc-len))	; return a list
      (else (slib:error "illegal format in format:parse-float")))))


(define format:zero-rep (char->integer #\0))

(define (format:round-numstr numstr dig) ; rounds a dot/signless number string
  (do ((i dig (- i 1))			; "099",2 -> "10",0
       (c 5))				; "023",2 -> "02",0
      ((or (= c 0) (< i 0))		; "999",2 -> "100",1 (carry)
       (if (= c 1)			; "005",2 -> "01,0"
	   (list (string-append "1" (substring numstr 0 dig)) 1)
	   (list (substring numstr 0 dig) 0)))
    (set! c (+ (- (char->integer (string-ref numstr i)) format:zero-rep) c))
    (string-set! numstr i (integer->char (if (< c 10) 
					     (+ c format:zero-rep)
					     (+ (- c 10) format:zero-rep))))
    (set! c (if (< c 10) 0 1))))


(define (format:strip-trailing-zeros numstr dig numlen) ; up to dig but one
  (let* ((numstr (string-append numstr "0")))
    (let loop ((i numlen))
      (if (and (char=? (string-ref numstr i) #\0)
	       (> i dig))
	  (loop (- i 1))
	  (substring numstr 0 (+ i 1))))))


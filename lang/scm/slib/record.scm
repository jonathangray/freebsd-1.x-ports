; Record.scm.  This more or less implements the records that are
; proposed for R5RS - unfortunately, all records created in this
; manner look like vectors.  I believe the original record proposal
; was made by Jonathan Rees.  This implementation defines some symbols
; other than those that are part of the record proposal - this
; wouldn't be a problem if Scheme had a module system, but it doesn't.

; Written by David Carlton, carlton@husc.harvard.edu.  This code is in
; the public domain.
; Extensively Modified for SLIB by Aubrey Jaffer, jaffer@ai.mit.edu.
; May 17 1992, MAKE-RECORD-SUB-TYPE added by jaffer.

(require 'common-list-functions)

; Tags to help identify rtd's.  (A record is identified by the rtd
; that begins it.)
(define record:*rtd-tag* (cons 'rtd '()))

; Checks to see if a list has any duplicates.  Also checks to see if
; it a list, for that matter.
(define (record:has-duplicates? lst)
  (cond
   ((null? lst) #f)
   ((not (pair? lst)) #t)
   ((memq (car lst) (cdr lst)) #t)
   (else (record:has-duplicates? (cdr lst)))))

; Various accessor functions.  No error checking; if you call these,
; you should know that they will work.
(define (record:rtd-tag x) (vector-ref x 0))
(define (record:rtd-name rtd) (vector-ref rtd 1))
(define (record:rtd-supers rtd) (vector-ref rtd 2))
(define (record:rtd-fields rtd) (vector-ref rtd 3))
;; rtd-vfields is padded out to the length of the vector, which is 1
;; more than the number of fields
(define (record:rtd-vfields rtd) (cons #f (record:rtd-fields rtd)))
;; rtd-length is the length of the vector.
(define (record:rtd-length rtd) (vector-ref rtd 4))

(define (record:record-rtd x) (vector-ref x 0))
(define (record:record-supers x) (vector-ref (vector-ref x 0) 2))

(define (record-predicate rtd)
  (if (not (record:rtd? rtd))
      (slib:error "record-predicate: invalid argument." rtd))
  (vector-ref rtd 5))

(define (record-sub-predicate rtd)
  (if (not (record:rtd? rtd))
      (slib:error "record-predicate: invalid argument." rtd))
  (vector-ref rtd 6))

(define (make-record-type type-name field-names)
  (if (not (string? type-name))
      (slib:error "make-record-type: non-string type-name argument."
		  type-name))
  (if (or (record:has-duplicates? field-names)
	  (comlist:notevery symbol? field-names))
      (slib:error "make-record-type: illegal field-names argument."
		  field-names))
  (let* ((corrected-length (+ 1 (length field-names)))
	 (rtd (vector record:*rtd-tag*
		      type-name
		      '()
		      field-names
		      corrected-length
		      #f
		      #f)))
    (vector-set! rtd 5
		 (lambda (x)
		   (and (vector? x)
			(= (vector-length x) corrected-length)
			(eq? (record:record-rtd x) rtd))))
    (vector-set! rtd 6
		 (lambda (x)
		   (and (vector? x)
			(>= (vector-length x) corrected-length)
			(or (eq? (record:record-rtd x) rtd)
			    (memq rtd (record:record-supers x)))
			#t)))
    rtd))

(define (make-record-sub-type type-name field-names rtd)
  (if (not (string? type-name))
      (slib:error "make-record-sub-type: non-string type-name argument."
		  type-name))
  (if (not (record:rtd? rtd))
      (slib:error "make-record-sub-type: non-rtd rtd argument."
		  rtd))
  (let ((xfield-names (append (record:rtd-fields rtd) field-names)))
    (if (or (record:has-duplicates? xfield-names)
	    (comlist:notevery symbol? field-names))
	(slib:error "make-record-sub-type: illegal field-names argument."
		    field-names))
    (let* ((corrected-length (+ 1 (length xfield-names)))
	   (rtd (vector record:*rtd-tag*
			type-name
			(cons rtd (record:rtd-supers rtd))
			xfield-names
			corrected-length
			#f
			#f)))
      (vector-set! rtd 5
		   (lambda (x)
		     (and (vector? x)
			  (= (vector-length x) corrected-length)
			  (eq? (record:record-rtd x) rtd))))
      (vector-set! rtd 6
		   (lambda (x)
		     (and (vector? x)
			  (>= (vector-length x) corrected-length)
			  (or (eq? (record:record-rtd x) rtd)
			      (memq rtd (record:record-supers x))))))
      rtd)))

; Determines whether or not a certain object looks like an rtd.
; Doesn't do as much error-checking as it could, but it would be quite
; unlikely for somebody to accidentally fool this function.
(define (record:rtd? object)
  (and (vector? object)
       ;; Could check for the exact value here, but then I'd have to
       ;; keep changing this as I change the format of a rtd.  This
       ;; is good enough to get the vector-ref to work.
       (not (= (vector-length object) 0))
       (eq? (record:rtd-tag object) record:*rtd-tag*)))

(define (record-constructor rtd . field-names)
  (if (not (record:rtd? rtd))
      (slib:error "record-constructor: illegal rtd argument." rtd))
  (if (or (null? field-names)
	  (equal? field-names (record:rtd-fields rtd)))
      (let ((record-length (- (record:rtd-length rtd) 1)))
	(lambda elts
	  (if (= (length elts) record-length) #t
	      (slib:error "record-constructor: "
			  (record:rtd-name rtd)
			  ": wrong number of arguments."))
	  (apply vector rtd elts)))
      (let ((record-vfields (record:rtd-vfields rtd))
	    (corrected-record-length (record:rtd-length rtd))
	    (field-names (car field-names)))
	(if (or (record:has-duplicates? field-names)
		(comlist:notevery (lambda (x) (memq x record-vfields))
			  field-names))
	    (slib:error
	     "record-constructor: invalid field-names argument."
	     (cdr record-vfields)))
	(let ((field-length (length field-names))
	      (offsets
	       (map (lambda (field) (comlist:position field record-vfields))
		    field-names)))
	  (lambda elts
	    (if (= (length elts) field-length) #t
		(slib:error "record-constructor: "
			    (record:rtd-name rtd)
			    ": wrong number of arguments."))
	    (let ((result (make-vector corrected-record-length)))
	      (vector-set! result 0 rtd)
	      (for-each (lambda (offset elt)
			  (vector-set! result offset elt))
			offsets
			elts)
	      result))))))

(define (record-accessor rtd field-name)
  (if (not (record:rtd? rtd))
      (slib:error "record-accessor: invalid rtd argument." rtd))
  (let ((index (comlist:position field-name (record:rtd-vfields rtd)))
	(corrected-length (record:rtd-length rtd)))
    (if (not index)
	(slib:error "record-accessor: invalid field-name argument."
		    field-name))
    (lambda (x)
      (if (and (vector? x)
	       (>= (vector-length x) corrected-length)
	       (or (eq? rtd (record:record-rtd x))
		   (memq rtd (record:record-supers x))))
	  #t
	  (slib:error "record-accessor: wrong record type." x "not" rtd))
      (vector-ref x index))))

(define (record-modifier rtd field-name)
  (if (not (record:rtd? rtd))
      (slib:error "record-modifier: invalid rtd argument." rtd))
  (let ((index (comlist:position field-name (record:rtd-vfields rtd)))
	(corrected-length (record:rtd-length rtd)))
    (if (not index)
	(slib:error "record-modifier: invalid field-name argument."
		    field-name))
    (lambda (x y)
      (if (and (vector? x)
	       (>= (vector-length x) corrected-length)
	       (or (eq? rtd (record:record-rtd x))
		   (memq rtd (record:record-supers x))))
	  #t
	  (slib:error "record-modifier: wrong record type." x "not" rtd))
      (vector-set! x index y))))

(define (record? obj)
  (and (vector? obj)
       (>= (vector-length obj) 1)
       (record:rtd? (record:record-rtd obj))
       (= (vector-length obj)
	  (record:rtd-length (record:record-rtd obj)))))

(define (record-type-descriptor record)
  (if (not (record? record))
      (slib:error "record-type-descriptor: invalid argument."
		  record))
  (record:record-rtd record))

(define (record-type-name rtd)
  (if (not (record:rtd? rtd))
      (perror "record-type-name: invalid argument."))
  (record:rtd-name rtd))

; For this function, make a copy of the value returned in order to
; make it a bit harder for the user to screw things up.
(define (record-type-field-names rtd)
  (if (not (record:rtd? rtd))
      (slib:error "record-type-field-names: invalid argument." rtd))
  (append (record:rtd-fields rtd) '()))

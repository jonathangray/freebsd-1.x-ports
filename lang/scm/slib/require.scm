;;;; Implementation of VICINITY and MODULES for Scheme
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

;;;; WARNING: this code redefines LOAD.

(define (user-vicinity)
  (case (software-type)
    ((VMS)	"[.]")
    (else	"")))

(define program-vicinity
  (let ((*vicinity-suffix*
	 (case (software-type)
	   ((NOSVE)	'(#\: #\.))
	   ((AMIGA)	'(#\: #\/))
	   ((UNIX)	'(#\/))
	   ((VMS)	'(#\: #\]))
	   ((MSDOS ATARIST OS/2)	'(#\\))
	   ((MACOS THINKC)	'(#\:)))))
    (lambda ()
      (let loop ((i (- (string-length *load-pathname*) 1)))
	(cond ((negative? i) "")
	      ((memv (string-ref *load-pathname* i)
		     *vicinity-suffix*)
	       (substring *load-pathname* 0 (+ i 1)))
	      (else (loop (- i 1))))))))

(define sub-vicinity
  (case (software-type)
    ((VMS)
     (lambda
      (vic name)
      (let ((l (string-length vic)))
	(if (or (zero? (string-length vic))
		(not (char=? #\] (string-ref vic (- l 1)))))
	    (string-append vic "[" name "]")
	    (string-append (substring vic 0 (- l 1))
			   "." name "]")))))
    (else
     (let ((*vicinity-suffix*
	    (case (software-type)
	      ((NOSVE) ".")
	      ((UNIX AMIGA) "/")
	      ((MACOS THINKC) ":")
	      ((MSDOS ATARIST OS/2) "\\"))))
       (lambda (vic name)
	 (string-append vic name *vicinity-suffix*))))))

(define in-vicinity string-append)

(define (make-vicinity <pathname>) <pathname>)

(define *catalog*
  (map
   (lambda (p)
     (if (symbol? (cdr p)) p
	 (cons
	  (car p)
	  (if (pair? (cdr p))
	      (cons 
	       (cadr p)
	       (in-vicinity (library-vicinity) (cddr p) (scheme-file-suffix)))
	      (in-vicinity (library-vicinity) (cdr p) (scheme-file-suffix))))))
   '(
     (rev4-optional-procedures	.	"sc4opt")
     (rev3-procedures		.	"sc3")
     (rev2-procedures		.	"sc2")
     (multiarg/and-		.	"mularg")
     (multiarg-apply		.	"mulapply")
     (rationalize		.	"ratize")
     (transcript		.	"trnscrpt")
     (with-file			.	"withfile")
     (dynamic-wind		.	"dynwind")
     (dynamic			.	"dynamic")
     (fluid-let		macro	.	"fluidlet")
     (alist			.	"alist")
     (hash			.	"hash")
     (hash-table		.	"hashtab")
     (logical			.	"logical")
     (random			.	"random")
     (random-inexact		.	"randinex")
     (modular			.	"modular")
     (prime			.	"prime")
     (charplot			.	"charplot")
     (sort			.	"sort")
     (common-list-functions	.	"comlist")
     (tree			.	"tree")
     (format			.	"format")
     (format-inexact		.	"formatfl")
     (generic-write		.	"genwrite")
     (pretty-print		.	"pp")
     (pprint-file		.	"ppfile")
     (object->string		.	"obj2str")
     (string-case		.	"strcase")
     (stdio			.	"stdio")
     (line-i/o			.	"lineio")
     (string-port		.	"strport")
     (getopt			.	"getopt")
     (debug			.	"debug")
;     (eval			.	"eval")
     (record			.	"record")
     (promise			.	"promise")
     (synchk			.	"synchk")
     (defmacro			.	"defmacro")
     (syntax-case		.	"scainit")
     (syntactic-closures	.	"scmacro")
     (macros-that-work		.	"macwork")
     (macro			.	macros-that-work)
     (yasos		macro	.	"yasos")
     (oop			.	yasos)
     (collect		macro	.	"collect")
     (struct	defmacro	.	"struct")
     (structure	syntax-case	.	"structure")
     (values			.	"values")
     (queue			.	"queue")
     (priority-queue		.	"priorque")
     (array			.	"array")
     (array-for-each		.	"arraymap")
     (repl			.	"repl")
     (process			.	"process")
     (test			.	"test")
     (red-black-tree		.	"rbtree")
     )))

(set! *catalog*
      (cons (cons 'portable-scheme-debugger
		  (in-vicinity (sub-vicinity (library-vicinity) "psd")
			       "psd-slib"
			       (scheme-file-suffix)))
	    *catalog*))

(define *load-pathname* #f)

(define base:load			;WARNING: redefining LOAD
  (let ((*old-load* load))
    (lambda (<pathname> . extra)
      (let ((old-load-pathname *load-pathname*))
	(set! *load-pathname* <pathname>)
	(apply *old-load* (cons <pathname> extra))
	(require:provide <pathname>)
	(set! *load-pathname* old-load-pathname)))))
(define load base:load)

;;;; MODULES

(define *modules* '())

(define (require:provided? feature)
  (if (symbol? feature)
      (if (memq feature *features*) #t
	  (let ((path (cdr (or (assq feature *catalog*) '(#f . #f)))))
	    (and path (member path *modules*) #t)))
      (and (member feature *modules*) #t)))

(define (require:feature->path feature)
  (if (symbol? feature)
      (if (memq feature *features*) #t
	  (let ((path (cdr (or (assq feature *catalog*) '(#f . #f)))))
	    (cond ((not path)
		   (set! feature (symbol->string feature))
		   (if (member feature *modules*) #t
		       feature))
		  ((symbol? path) (require:feature->path path))
		  ((member (if (pair? path) (cdr path) path) *modules*)
		   #t)
		  (else path))))
      (if (member feature *modules*) #t
	  feature)))

(define (require:require feature)
  (let ((path (require:feature->path feature)))
    (cond ((eq? path #t) #t)
	  ((not path)
	   (newline)
	   (display ";required feature not supported: ")
	   (display feature)
	   (newline)
	   (slib:error ";required feature not supported: " feature))
	  ((pair? path)
	   (require (car path))
	   ((case (car path)
	      ((macro) macro:load)
	      ((syntactic-closures) synclo:load)
	      ((syntax-case) syncase:load)
	      ((macros-that-work) macwork:load)
	      ((defmacro) defmacro:load))
	    (cdr path))
	   (require:provide feature))
	  (else
	   (base:load path)
	   (require:provide feature)))))

(define (require:provide feature)
  (if (symbol? feature)
      (if (not (memq feature *features*))
	  (set! *features* (cons feature *features*)))
      (if (not (member feature *modules*))
	  (set! *modules* (cons feature *modules*)))))

(require:provide 'vicinity)

(define provide require:provide)
(define provided? require:provided?)
(define require require:require)

(if (inexact? (string->number "0.0")) (provide 'inexact))
(if (rational? (string->number "1/19")) (provide 'rational))
(if (real? (string->number "0.0")) (provide 'real))
(if (complex? (string->number "1+i")) (provide 'complex))
(if (exact? (string->number "9999999999999999999999999999999"))
    (provide 'bignum))

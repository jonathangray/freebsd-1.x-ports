;;;; Template for configuration of *features* for Scheme
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

;;; (software-type) should be set to the generic operating system type.
;;; UNIX, VMS, MACOS, AMIGA and MSDOS are supported.

(define (software-type) 'UNIX)

;;; *FEATURES* should be set to a list of symbols describing features
;;; of this implementation.  Suggestions for features are:

(define *features*
      '(
;	rev4-report			;conforms to
;	rev3-report			;conforms to
;	ieee-p1178			;conforms to
;	sicp				;runs code from Structure and
					;Interpretation of Computer
					;Programs by Abelson and Sussman.
;	rev4-optional-procedures	;LIST-TAIL, STRING->LIST,
					;LIST->STRING, STRING-COPY,
					;STRING-FILL!, LIST->VECTOR,
					;VECTOR->LIST, and VECTOR-FILL!
;	rev3-procedures			;LAST-PAIR, T, and NIL
;	rev2-procedures			;SUBSTRING-MOVE-LEFT!,
					;SUBSTRING-MOVE-RIGHT!,
					;SUBSTRING-FILL!,
					;STRING-NULL?, APPEND!, 1+,
					;-1+, <?, <=?, =?, >?, >=?
;	multiarg/and-			;/ and - can take more than 2 args.
;	multiarg-apply			;APPLY can take more than 2 args.
;	rationalize
;	delay				;has DELAY and FORCE
;	with-file			;has WITH-INPUT-FROM-FILE and
					;WITH-OUTPUT-FROM-FILE
;	string-port			;has CALL-WITH-INPUT-STRING and
					;CALL-WITH-OUTPUT-STRING
;	transcript			;TRANSCRIPT-ON and TRANSCRIPT-OFF
;	char-ready?
;	macro				;has R4RS high level macros
;	defmacro			;has Common Lisp DEFMACRO
;	eval				;SLIB:EVAL is single argument eval
;	record				;has user defined data structures
;	values				;proposed multiple values
;	dynamic-wind			;proposed dynamic-wind
;	ieee-floating-point		;conforms to
	full-continuation		;can return multiple times
;	object-hash			;has OBJECT-HASH

;	sort
;	queue				;queues
;	pretty-print
;	object->string
;	format
;	compiler			;has (COMPILER)
;	ed				;(ED) is editor
;	system				;posix (system <string>)
	getenv				;posix (getenv <string>)
;	program-arguments		;returns list of strings (argv)
;	Xwindows			;X support
;	curses				;screen management package
;	termcap				;terminal description package
;	terminfo			;sysV terminal description
	))

;;; (OUTPUT-PORT-WIDTH <port>)
(define (output-port-width . arg) 79)

;;; (OUTPUT-PORT-HEIGHT <port>)
(define (output-port-height . arg) 24)

;;; (CURRENT-ERROR-PORT)
(define current-error-port
  (let ((port (current-output-port)))
    (lambda () port)))

;;; (TMPNAM) makes a temporary file name.
(define tmpnam (let ((cntr 100))
		 (lambda () (set! cntr (+ 1 cntr))
			 (string-append "slib_" (number->string cntr)))))

;;; (FILE-EXISTS? <string>)
(define (file-exists? f) #f)

;;; (DELETE-FILE <string>)
(define (delete-file f) #f)

;;; FORCE-OUTPUT flushes any pending output on optional arg output port
;;; use this definition if your system doesn't have such a procedure.
(define (force-output . arg) #t)

;;; CALL-WITH-INPUT-STRING and CALL-WITH-OUTPUT-STRING are the string
;;; port versions of CALL-WITH-*PUT-FILE.

;;; CHAR-CODE-LIMIT is one greater than the largest integer which can
;;; be returned by CHAR->INTEGER.
(define char-code-limit 256)

;;; MOST-POSITIVE-FIXNUM is used in modular.scm
(define most-positive-fixnum #x0FFFFFFF)

;;; If your implementation provides eval SLIB:EVAL is single argument
;;; eval using the top-level (user) environment.
;(define slib:eval eval)

;;; If your implementation provides R4RS macros:
;(define macro:eval slib:eval)
;(define macro:load load)

;;; define an error procedure for the library
;(define slib:error error)

;;; define these as appropriate for your system.
(define slib:tab (integer->char 9))
(define slib:form-feed (integer->char 12))

;;; Define these if your implementation's syntax can support it and if
;;; they are not already defined.

;(define (1+ n) (+ n 1))
;(define (-1+ n) (+ n -1))
;(define 1- -1+)

;;; (implementation-vicinity) should be defined to be the pathname of
;;; the directory where any auxillary files to your Scheme
;;; implementation reside.

(define (implementation-vicinity)
  (case (software-type)
    ((UNIX)	 "/usr/local/src/scheme/")
    ((VMS)	"scheme$src:")
    ((MSDOS)	"C:\\scheme\\")))

;;; (library-vicinity) should be defined to be the pathname of the
;;; directory where files of Scheme library functions reside.

(define library-vicinity
  (let ((library-path
	 (or (and (memq 'getenv *features*)
		  (getenv "SCHEME_LIBRARY_PATH"))
;;; Uses this path if your scheme does not support GETENV.
	     (case (software-type)
	       ((UNIX) "/usr/local/lib/slib/")
	       ((VMS) "lib$scheme:")
	       ((MSDOS) "C:\\SLIB\\")
	       (else "")))))

    (lambda () library-path)))

(define scheme-file-suffix
  (let ((suffix (case (software-type)
		  ((NOSVE) "_scm")
		  (else ".scm"))))
    (lambda () suffix)))

(define in-vicinity string-append)

(load (in-vicinity (library-vicinity) "require" (scheme-file-suffix)))

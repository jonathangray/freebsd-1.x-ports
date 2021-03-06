;;; This is when the actual code generation occurs.  It is entered with a
;;; list of expressions.  Code is not as optimal as it might be, but then
;;; that's what the C compiler is for.
;;;

;*           Copyright 1989-1993 Digital Equipment Corporation
;*                         All Rights Reserved
;*
;* Permission to use, copy, and modify this software and its documentation is
;* hereby granted only under the following terms and conditions.  Both the
;* above copyright notice and this permission notice must appear in all copies
;* of the software, derivative works or modified versions, and any portions
;* thereof, and both notices must appear in supporting documentation.
;*
;* Users of this software agree to the terms and conditions set forth herein,
;* and hereby grant back to Digital a non-exclusive, unrestricted, royalty-free
;* right and license under any changes, enhancements or extensions made to the
;* core functions of the software, including but not limited to those affording
;* compatibility with other hardware or software environments, but excluding
;* applications which incorporate this software.  Users further agree to use
;* their best efforts to return to Digital any such changes, enhancements or
;* extensions that they make and inform Digital of noteworthy uses of this
;* software.  Correspondence should be provided to Digital at:
;* 
;*                       Director of Licensing
;*                       Western Research Laboratory
;*                       Digital Equipment Corporation
;*                       250 University Avenue
;*                       Palo Alto, California  94301  
;* 
;* This software may be distributed (but not offered for sale or transferred
;* for compensation) to third parties, provided such third parties agree to
;* abide by the terms and conditions of this notice.  
;* 
;* THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
;* WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
;* MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
;* CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;* DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;* PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
;* ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;* SOFTWARE.

(module gencode)

;;; External and in-line declarations.

(include "plist.sch")
(include "expform.sch")
(include "lambdaexp.sch")
(include "miscexp.sch")
(include "lap.sch")

;;; Top-level globals.

(define CURRENT-CODE-LAMBDA 'top-level)

(define CURRENT-DEFINE-STRING '())	; id which is the string defining the
					; current top-level DEFINE.

(define INIT-MODULES '())

(define FREE-DISPLAY 0)

(define MAX-DISPLAY 0)

(define EMPTY-CONDITION-INFO '(()) )

(define GLOBAL-CONDITION-INFO empty-condition-info)

(define ERROR-ID #f)

(define $_CAR-ERROR-ID #f)

(define $_CDR-ERROR-ID #f)

(define (GENERATE-CODE expl)
    (let ((bindings '())
	  (initname (if main-program-name
			"main"
			(string-append module-name "__init")))
	  (constant-lap '()))
	 (set! current-code-lambda 'top-level)
	 (set! current-define-name 'top-level)
	 (save-current-lap #f)
	 (set! max-display 0)
	 (set! free-display 0)
	 (set! error-id (bound 'error))
	 (set! $_car-error-id (bound '$_car-error))
	 (set! $_cdr-error-id (bound '$_cdr-error))
	 (set! global-lap-code '())
	 (emit-global-lap `(LIT "/* SCHEME->C */"))
	 (emit-global-lap `(LIT))
	 (emit-global-lap `(LIT "#include " #\< ,c-include-file #\>))
         (emit-global-lap '(LIT))
	 (set! init-modules '())
	 (if main-program-name
	     (begin (emit-lap '(LIT "main( argc, argv )"))
		    (emit-lap '(LIT "        int argc;  char *argv[];")))
	     (emit-lap `(LIT "void  " ,initname "()")))
	 (emit-lap '(LIT "{"))
	 (emit-lap '(indent 8))
	 (emit-lap '(LOCALS DISPLAY 0))
	 (emit-lap '(LIT "static int  init = 0;"))
	 (emit-lap '(LIT "if  (init)  return;"))
	 (emit-lap '(LIT "init = 1;"))
	 (if main-program-name
	     (emit-lap `(INITHEAP ,heap-size "argc" "argv"
			    ,(cname (id-global main-program-name))))
	     (emit-lap `(INITHEAP ,heap-size 0 0 0)))
	 (emit-lap '(LIT "init_constants();"))
	 (set! constant-lap (emit-constants))
	 (done-lap constant-lap)
	 (emit-lap
	     `(LIT "init_modules( "
		   (CSTRING ,(string-append "(" module-name
				 " SCHEME->C COMPILER " scc-version ")"))
		   " );"))
	 (for-each (lambda (exp) 
		     (set! global-condition-info empty-condition-info)
		     (exp-genc 'no-value exp bindings))
		   expl)
	 (if main-program-name
	     (let ((name (id-global main-program-name)))
		  (emit-global-lap
		      `(LIT "void  " ,module-name "__init(){}"))
		  (if name
		      (emit-lap `(LIT ,(cname name)
				      "( CLARGUMENTS( argc, argv ) );"))
		      (report-error "Main procedure is not defined"))
		  (emit-lap '(LIT "SCHEMEEXIT();")))
	     (emit-lap '(SET RETURN "void")))
	 (emit-lap '(indent 0))
	 (emit-lap '(LIT "}"))
	 (if (not (= 0 free-display))
	     (report-error "Compiler error - display index is not 0"))
	 (generate-init_modules)
	 (done-lap (save-current-lap '()))))

;;; Code for each expression is generated by the following function.  It
;;; returns the code which evaluates to the expression.

(define (EXP-GENC loc exp bindings)		
    (cond ((symbol? exp) 	    (symbol-genc loc exp bindings))
	  ((eq? (car exp) '$call)   ($call-genc loc exp bindings))
	  ((eq? (car exp) '$set)    ($set-genc loc exp bindings))
	  ((eq? (car exp) '$lambda) ($lambda-genc loc exp bindings))
	  ((eq? (car exp) '$if)	    ($if-genc loc exp bindings))
	  ((eq? (car exp) '$define) ($define-genc loc exp bindings))
	  ((eq? (car exp) '$lap)    (report-error "Illegal use of LAP"))
	  (else
	   (report-error "GENERATE-CODE compiler error" exp))))	   

;;; Labels are needed during the code generation and are constructed by the
;;; following function.  ID-BOUNDREFS is used to keep track of the number of
;;; references.

(define (MAKE-LABEL) (newv 'l 'use 'label 'gotos 0))

;;; Code labels are automatically constructed for all lambda expressions by
;;; the following function.  Labels that are not used are removed during
;;; peep-hole optimization of the lap code.

(define (CODE-LABEL id)
    (let ((label (lambda-code-label id)))
	 (if (not label)
	     (begin (set! label (make-label))
		    (set-lambda-code-label! id label)))
	 label))

;;; Global names are sometimes needed in the C-code and are emitted by the
;;; following function.

(define (MAKE-C-GLOBAL)
    (newv 'temp 'use 'temporary))
	  
;;; The optional argument (if any) of a function is returned by the following
;;; function.

(define (OPTIONAL-ARGS id)
    (if (lambda-optvars id)
        (car (lambda-optvars id))
	'()))

;;; Variables are "looked-up" in the current bindings by the following
;;; function.  It returns the code the access the value bound to the
;;; variable.

(define (LOOKUP var bindings)
    (let ((offset 0)
	  (code '()))
	 (cond ((var-is-constant var)
		(vname var))
	       ((var-is-global var)
		(emit-extern var)
		(or (vname var)
		    (and (id-type var) (cname var))
		    (report-error "SYMBOL does not have a value cell"
			(id-printname var))))
	       ((var-in-stack var)
		(let ((displayx (id-display var)))
		     (cond ((id-set! var)
			    `(PAIR_CAR ,(if displayx
					    `("DISPLAY" ,displayx)
					    (vname var))))
			   (displayx `("DISPLAY" ,displayx))
			   (else (vname var)))))
	       ((var-is-top-level var)
		`(SYMBOL_VALUE ,(vname var)))
	       (else (report-error "Variable is not bound" (vname var))))))

(define (VAR-IN-STACK var)
    (eq? (id-use var) 'lexical))

(define (VAR-IS-GLOBAL var)
    (eq? (id-use var) 'global))

(define (VAR-IS-CONSTANT var) (eq? (id-use var) 'constant))

(define (VAR-IS-TOP-LEVEL var) (eq? (id-use var) 'top-level))

;;; Emit declarations for external procedures.

(define (EMIT-EXTERN var)
    (if (id-lambda var)
	(set! var (lambda-name (id-lambda var))))
    (when (not (id-external var))
	  (set-id-external! var #t)
	  (cond ((and (id-lambda var)
		      (assq (id-type var)
			    '((void . EXTERNVOIDP)
			      (pointer . EXTERNPOINTERP)
			      (array . EXTERNARRAYP)
			      (tscp . EXTERNTSCPP)
			      (char . EXTERNCHARP)
			      (int . EXTERNINTP)
			      (shortint .  EXTERNSHORTINTP)
			      (longint . EXTERNLONGINTP)
			      (unsigned . EXTERNUNSIGNEDP)
			      (shortunsigned . EXTERNSHORTUNSIGNEDP)
			      (longunsigned . EXTERNLONGUNSIGNEDP)
			      (float . EXTERNFLOATP)
			      (double . EXTERNDOUBLEP))))
		 => (lambda (type)
			    (emit-global-lap `(,(cdr type) ,(cname var)
					       ,(emit-c-extern-argl
						    (id-lambda var))))))
		((assq (id-type var)
		       '((pointer . EXTERNPOINTER)
			 (array . EXTERNARRAY)
			 (tscp . EXTERNTSCP)
			 (char . EXTERNCHAR)
			 (int . EXTERNINT)
			 (shortint .  EXTERNSHORTINT)
			 (longint . EXTERNLONGINT)
			 (unsigned . EXTERNUNSIGNED)
			 (shortunsigned . EXTERNSHORTUNSIGNED)
			 (longunsigned . EXTERNLONGUNSIGNED)
			 (float . EXTERNFLOAT)
			 (double . EXTERNDOUBLE)))
		 => (lambda (type)
			    (emit-global-lap `(,(cdr type) ,(vname var)))))
		(else
		 (if (id-lambda var)
		     (emit-global-lap
			 `(EXTERNTSCPP ,(cname var)
			      ,(emit-extern-argl (id-lambda var)))))
		 (if (var-is-global var)
		     (let ((vmodule (id-module var)))
			  (if (vname var)
			      (emit-global-lap `(EXTERNTSCP ,(vname var))))
			  (if (and (not (equal? module-name vmodule))
				   (not (member vmodule '("" "sc")))
				   (not (member vmodule init-modules)))
			      (set! init-modules
				    (cons (id-module var) init-modules)))))))))

(define (EMIT-C-EXTERN-ARGL lid)
    
    (define XALN
	    '#(XAL0 XAL1 XAL2 XAL3 XAL4 XAL5 XAL6 XAL7 XAL8 XAL9
		    XAL10 XAL11 XAL12 XAL13 XAL14 XAL15 XAL16 XAL17 XAL18
		    XAL19 XAL20 XAL21 XAL22 XAL23 XAL24 XAL25 XAL26))
    
    (define (CTYPES vars)
	    (map (lambda (var)
			 (cdr (assq var '((void . "void")
					  (pointer . "void*")
					  (array . "void*")
					  (tscp . "TSCP")
					  (char . "char")
					  (int . "int")
					  (shortint . "short int")
					  (longint . "long int")
					  (unsigned . "unsigned")
					  (shortunsigned . "short unsigned")
					  (longunsigned . "long unsigned")
					  (float . "float")
					  (double . "double")))))
		 vars))
    
    (let ((req (lambda-reqvars lid))
	  (opt (lambda-optvars lid)))
	 (if opt
	     `(,(vector-ref xaln (+ (length req) 2))
	       ,@(ctypes req) ,@(ctypes opt) "...")
	     `(,(vector-ref xaln (length req)) ,@(ctypes req)))))

(define (EMIT-EXTERN-ARGL lid)
    
    (define XALN
	    '#(XAL0 XAL1 XAL2 XAL3 XAL4 XAL5 XAL6 XAL7 XAL8 XAL9
		    XAL10 XAL11 XAL12 XAL13 XAL14 XAL15 XAL16 XAL17 XAL18
		    XAL19 XAL20 XAL21 XAL22 XAL23 XAL24 XAL25 XAL26))
    
    (let ((count (+ (length (lambda-reqvars lid))
		    (length (lambda-optvars lid))
		    (if (lambda-closed lid) 1 0))))
	 (cons (vector-ref xaln count)
	       (let loop ((i count))
		    (if (zero? i)
			'()
			(cons 'tscp (loop (- i 1))))))))

;;; When all code has been emitted, this function is called to emit the
;;; procedure "init_modules" which calls the initialization code for all
;;; modules used by this program.

(define (GENERATE-INIT_MODULES)
    (let ((save-lap (save-current-lap '())))
	 (emit-lap '(LIT "static void  init_modules( compiler_version )"))
	 (emit-lap '(LIT "        char *compiler_version;"))
	 (emit-lap '(LIT"{"))
	 (emit-lap '(indent 8))
	 (for-each
	     (lambda (with-module)
		     (emit-lap
			 `(LIT ,(string-append  with-module "__init();"))))
	     (append init-modules with-modules))
	 (emit-lap `(MAXDISPLAY ,max-display))
	 (emit-lap '(indent 0))
	 (emit-lap `(LIT "}"))
	 (done-lap (save-current-lap save-lap))))

;;; All storage and initialization for constants is emitted at the start of
;;; the module's initialization function.  Since vectors, strings, lists, and
;;; floating point numbers are constructed from the heap, they must be
;;; registered with the run-time system.

(define CONSTANT-SYMBOLS '())

(define CONSTANT-SYMBOL-PORT '())

(define (EMIT-CONSTANTS)
    (let ((save-lap (save-current-lap '())))
	 (set! constant-symbols '())
	 (set! constant-symbol-port (open-output-string))
	 (emit-lap '(LIT "static void  init_constants()"))
	 (emit-lap '(LIT "{"))
	 (emit-lap '(INDENT 8))
	 (emit-lap '(LOCALS))
	 (for-each
	     (lambda (const-var)
		     (let ((var (cadr const-var))
			   (const (car const-var))
			   (temps (save-lap-temps)))
			  (emit-constant var const)
			  (if (and (not (fixed? const)) (not (char? const)))
			      (emit-lap `(CONSTANTEXP (ADR ,(vname var)))))
			  (restore-lap-temps temps)))
	     quote-constants)
	 (emit-lap '(INDENT 0))
	 (emit-lap '(LIT "}"))
	 (save-current-lap save-lap)))

(define (EMIT-CONSTANT var const)
    (cond ((fixed? const)
	   (display "_TSCP( " constant-symbol-port)
	   (if (or (> const 2) (< const -2))
	       (begin (write (+ (* 4 (quotient const 10))
				(quotient (* 4 (remainder const 10)) 10))
			     constant-symbol-port)
		      (write (abs (remainder (* 4 (remainder const 10)) 10))
			     constant-symbol-port))
	       (write (remainder (* 4 (remainder const 10)) 10)
		      constant-symbol-port))
	   (display " )" constant-symbol-port)
	   (set-id-vname! var (get-output-string constant-symbol-port)))
	  ((float? const)
	   (emit-global-lap `(DEFSTATICTSCP ,(vname var)))
	   (emit-lap `(SET ,(vname var) (DOUBLE_TSCP ,const))))
	  ((char? const)
	   (display "_TSCP( " constant-symbol-port)
	   (write (+ (* (char->integer const) 256) 18)
		  constant-symbol-port)
	   (display " )" constant-symbol-port)
	   (set-id-vname! var (get-output-string constant-symbol-port)))
	  ((string? const)
	   (let ((temp (make-c-global)))
		(emit-global-lap `(DEFCSTRING ,(vname temp) (CSTRING ,const)))
		(emit-global-lap `(DEFSTATICTSCP ,(vname var)))
		(emit-lap `(SET ,(vname var) (CSTRING_TSCP ,(vname temp))))))
	  ((symbol? const)
	   (emit-global-lap `(DEFSTATICTSCP ,(vname var)))
	   (emit-lap `(SET ,(vname var)
			   (STRINGTOSYMBOL
			       (CSTRING_TSCP
				   (CSTRING ,(symbol->string const))))))
	   (set! constant-symbols (cons (list const var) constant-symbols)))
	  ((pair? const)
	   (if (eq? (id-use var) 'constant)
	       (emit-global-lap  `(DEFSTATICTSCP ,(vname var))))
	   (emit-constant-list (vname var) const))
	  ((vector? const)
	   (emit-constant var (vector->list const))
	   (emit-lap `(SET ,(vname var) (LISTTOVECTOR ,(vname var)))))
	  (else (report-error "EMIT-CONSTANT compile error:" const))))

(define (EMIT-CONSTANT-LIST varname const)
    (cond ((pair? const)
	   (emit-constant-list varname (cdr const))
	   (emit-lap `(SET ,varname
			   (CONS ,(emit-constant-element (car const))
				 ,varname))))
	  (else
	       (emit-lap `(SET ,varname
			       ,(emit-constant-element const))))))

(define (EMIT-CONSTANT-ELEMENT const)
    (cond ((eq? const #t) "TRUEVALUE" )
	  ((eq? const '()) "EMPTYLIST" )
	  ((eq? const #f) "FALSEVALUE")
	  ((equal? const "") "EMPTYSTRING")
	  ((equal? const '#()) "EMPTYVECTOR")
	  ((or (fixed? const) (char? const))
	   (emit-constant 'emit-constant-kludge const))
	  ((pair? const)
	   (let ((temp (use-lap-temp)))
		(emit-constant-list (vname temp) const)
		(drop-lap-temp temp)
		(id-vname temp)))
	  ((vector? const)
	   (let ((temp (use-lap-temp)))
		(emit-constant temp const)
		(drop-lap-temp temp)
		(id-vname temp)))	   
	  ((and (symbol? const) (assq const constant-symbols))
	   => (lambda (symbol.const) (vname (cadr symbol.const))))
	  (else
	   (let ((temp (make-c-global)))
		(emit-constant temp const)
		(id-vname temp)))))

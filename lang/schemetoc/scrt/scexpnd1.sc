;;; This module contains the basic macro expanders required by Scheme.

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

(module scexpnd1 (top-level DO-DEFINE))

(include "repdef.sc")

;;; (DELAY exp)  ==>  (make-promise (lambda () exp))

(install-expander
    'DELAY
    (lambda (x e)
	    (if (islist x 2 2)
		(e `(make-promise (lambda () ,(cadr x))) e)
		(error 'delay "Illegal form: ~s" x))))

;;; (QUOTE exp)  ==>  (quote exp)

(install-expander
    'QUOTE
    (lambda (x e)
	    (if (islist x 2 2) x (error 'quote "Illegal form: ~s" x))))

;;; (DEFINE symbol exp)  ==>  		(do-define 'symbol exp)
;;; (DEFINE (symbol ...) exp ...)  ==>	(do-define 'symbol (lambda ...))

(define (DO-DEFINE symbol value)
    (let ((was (top-level-value symbol)))
	 (set-top-level-value! symbol value)
	 (if (not (eq? was $_undefined))
	     (display (format "***** ~s is redefined~%" symbol)))
	 symbol))

(install-expander
    'DEFINE
    (lambda (x e)
	    (cond ((and (islist x 3 3) (symbol? (cadr x)))
		   `(do-define (quote ,(cadr x)) ,(e (caddr x) e)))
		  ((and (islist x 3) (pair? (cadr x)) (symbol? (caadr x)))
		   `(do-define (quote ,(caadr x))
			       ,(e `(lambda ,(cdadr x) ,@(cddr x)) e)))
		  (else (error 'define "Illegal form: ~s" x)))))

;;; (DEFINE-IN-LINE (symbol ...) exp ...) ==> 'symbol
;;;
;;; N.B.  expanding this form causes the macro to be defined.

(install-expander
    'DEFINE-IN-LINE
     (lambda (x e)
	     (cond ((and (islist x 3) (pair? (cadr x)) (symbol? (caadr x)))
		    (let ((func (caadr x))
			  (args (cdadr x))
			  (body (cddr x)))
			 (install-expander
			     func
			     (eval `(lambda (x e)
					    (e (cons '(lambda ,args ,@body)
						     (cdr x))
					       e))))
			 `(quote ,func)))
		   (else (error 'define-in-line "Illegal form: ~s" x)))))

;;; (DEFINE-MACRO symbol expander) ==> 'symbol
;;;
;;; N.B.  expanding this form causes the macro to be defined.

(install-expander
    'DEFINE-MACRO
    (lambda (x e)
	    (cond ((and (islist x 3 3) (symbol? (cadr x)))
		   (install-expander (cadr x) (eval (caddr x)))
		   `(quote ,(cadr x)))
		  (else (error 'define-macro "Illegal form: ~s" x)))))

;;; (DEFINE-CONSTANT symbol value) ==> 'symbol
;;;
;;; N.B.  expanding this form causes the macro to be defined.

(install-expander
    'DEFINE-CONSTANT
    (lambda (x e)
	    (cond ((and (islist x 3 3) (symbol? (cadr x)))
		   (install-expander (cadr x) (list (eval (caddr x))))
		   `(quote ,(cadr x)))
		  (else	(error 'define-constant "Illegal form: ~s" x)))))

;;; (DEFINE-EXTERNAL ...)  ==>  '(DEFINE-EXTERNAL ...)

(install-expander 'DEFINE-EXTERNAL (lambda (x e) (e (list 'quote x) e)))

;;; (DEFINE-C-EXTERNAL ...)  ==>  '(DEFINE-C-EXTERNAL ...)

(install-expander 'DEFINE-C-EXTERNAL (lambda (x e) (e (list 'quote x) e)))

;;; (EVAL-WHEN situation form)  ==>  (begin form)
;;;				==>  #f

(install-expander
    'EVAL-WHEN
    (lambda (x e)
	    (if (and (islist x 3) (islist (cadr x) 1))
		(if (memq 'eval (cadr x))
		    (e `(begin ,@(cddr x)) e)
		    '#f)
		(error 'eval-when "Illegal form: ~s" x))))

;;; Trivial macro expanders for the basic forms evaluated by the interpreter
;;; are provided to do syntax checking at this point, rather than during
;;; interpretation.

;;; (LAMBDA args ...)  ==>  (lambda args ...)

(install-expander
    'LAMBDA
    (lambda (x e)
	    (let ((e (internal-begin-expander e)))
		 (if (islist x 3)
		     `(lambda ,(cadr x)
			      ,@(lambda-defines
				    (map (lambda (x) (e x e)) (cddr x))))
		     (error 'lambda "Illegal form: ~s" x)))))

;;; The following procedure is called to rewrite the body of any lambda
;;; expression which contains DEFINE's to an equivalent lambda form.

(define (LAMBDA-DEFINES body)
    (let loop ((oldforms body) (newforms '()) (vars '()) (sets '()))
	 (if (pair? oldforms)
	     (let ((form (car oldforms)))
		  (cond ((or (not (pair? form))
			     (not (eq? (car form) 'do-define)))
			 (loop (cdr oldforms) (cons form newforms)
			       vars sets))
			(else
			 (loop (cdr oldforms) newforms
			       (cons (cadadr form) vars)
			       (cons `(set! ,(cadadr form) ,(caddr form))
				     sets)))))
	     (if vars
		 `(((lambda ,vars ,@(reverse sets) ,@(reverse newforms))
		    ,@(vector->list (make-vector (length vars) 0))))
		 body))))

;;; (IF A B C)  ==> (if a b c)
;;; (IF A B)  ==>   (if a b #f)

(install-expander
    'IF
    (lambda (x e)
	    (cond ((islist x 3 3)
		   (list 'if (e (cadr x) e) (e (caddr x) e) #f))
		  ((islist x 4 4)
		   (list 'if (e (cadr x) e) (e (caddr x) e) (e (cadddr x) e)))
		  (else (error 'if "Illegal form: ~s" x)))))

;;; (SET! var value)  ==>  (set! var value)

(install-expander
    'SET!
    (lambda (x e)
	    (if (and (islist x 3 3) (symbol? (cadr x)))
		`(set! ,(cadr x) ,(e (caddr x) e))
		(error 'set! "Illegal form: ~s" x))))

;;; (BEGIN value ...)  ==>  (begin value ...)

(install-expander
    'BEGIN
    (lambda (x e)
	    (if (islist x 2)
		`(begin ,@(map (lambda (x) (e x e)) (cdr x)))
		(error 'begin  "Illegal form: ~s" x))))

(define (INTERNAL-BEGIN-EXPANDER old-expander)
    (lambda (expr expander)
	    (if (and (pair? expr) (eq? (car expr) 'begin))
		(if (islist expr 2)
		    `(begin ,@(lambda-defines
				  (map (lambda (x) (expander x expander))
				       (cdr expr))))
		    (error 'begin "Illegal form: ~s" expr))
		(old-expander expr expander))))

;;; Derived expression types are expanded in this module using the rules
;;; given in section 7.3 of Revised**3.

;;; Conditional forms are expanded into if sequences.

(define (COND-MACRO exp)
    (let* ((clauses  (cdr exp))
	   (clause1  (and clauses (car clauses)))
	   (clause2+ (and clause1 (cdr clauses))))
	  (cond ((null? clause1)
		 '#f)
		((or (not (pair? clause1)) (equal? clause1 '(else)))
		 (error 'cond-clause "Illegal form ~s:" exp))
		((null? (cdr clause1))
		 `(or ,(car clause1) (cond ,@clause2+)))
		((and (eq? (cadr clause1) '=>) (= (length clause1) 3))
		 `(let ((test-result ,(car clause1))
			(thunk2 (lambda () ,(caddr clause1)))
			(thunk3 (lambda () (cond ,@clause2+))))
		       (if test-result ((thunk2) test-result) (thunk3))))
		((eq? (car clause1) 'else)
		 `(begin ,@(cdr clause1)))
		(else `(if ,(car clause1)
			   (begin ,@(cdr clause1))
			   (cond ,@clause2+))))))

(install-expander 'COND (lambda (x e) (e (cond-macro x) e)))

(define (CASE-MACRO exp)
    (cond ((islist exp 3)
	   (do ((keyval (cadr exp))
		(key (string->uninterned-symbol "key"))
		(cases (cddr exp) (cdr cases))
		(ccs '()))
	       ((or (not (pair? cases)) (not (islist (car cases) 2)))
		(cond (cases
		       (error 'case "Illegal form: ~s" exp))
		      (else
		       `(let ((,key ,keyval)) (cond ,@(reverse ccs))))))
	       (cond ((eq? (caar cases) 'else)
		      (set! ccs (cons (car cases) ccs)))
		     (else
		      (set! ccs
			    (cons `((memv ,key (quote ,(caar cases)))
				    ,@(cdar cases))
				  ccs))))))
	  (else (error 'case "Illegal form:" exp))))

(install-expander 'CASE (lambda (x e) (e (case-macro x) e)))
		
(define (AND-MACRO exp)
    (cond ((null? (cdr exp))  '#t)
	  ((null? (cddr exp)) (cadr exp))
	  (else `(let ((x ,(cadr exp))
		       (thunk (lambda () (and ,@(cddr exp)))))
		      (if x (thunk) x)))))

(install-expander 'AND (lambda (x e) (e (and-macro x) e)))

(define (OR-MACRO exp)
    (cond ((null? (cdr exp))  '#f)
	  ((null? (cddr exp)) (cadr exp))
	  (else  `(let ((x ,(cadr exp))
			(thunk (lambda () (or ,@(cddr exp)))))
		       (if x x (thunk))))))

(install-expander 'OR (lambda (x e) (e (or-macro x) e)))

;;; (WHEN test exp ...)  ==>  (if test (begin exp ...))

(define (WHEN-MACRO exp)
    (if (islist exp 3)
	`(if ,(cadr exp) (begin ,@(cddr exp)))
	(error 'WHEN "Illegal form: ~s" exp)))

(install-expander 'WHEN (lambda (x e) (e (when-macro x) e)))

;;; (UNLESS test exp ...)  ==>  (if (not test) (begin exp ...))

(define (UNLESS-MACRO exp)
    (if (islist exp 3)
	`(if (not ,(cadr exp)) (begin ,@(cddr exp)))
	(error 'UNLESS "Illegal form: ~s" exp)))

(install-expander 'UNLESS (lambda (x e) (e (unless-macro x) e)))



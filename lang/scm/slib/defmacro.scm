;;;defmacro for any Scheme dialect 
;;;Copyright 1993 Dorai Sitaram and Aubrey Jaffer.

(define *macros*
  (list (cons 'defmacro
	      (lambda (name parms . body)
		`(set! *macros* (cons (cons ',name (lambda ,parms ,@body))
				      *macros*))))))
(define (macro? m) (and (assq m *macros*) #t))

(define (macroexpand-1 e)
  (if (pair? e) (let ((a (car e)))
		  (cond ((symbol? a) (set! a (assq a *macros*))
				     (if a (apply (cdr a) (cdr e)) e))
			(else e)))
      e))

(define (macroexpand e)
  (if (pair? e) (let ((a (car e)))
		  (cond ((symbol? a)
			 (set! a (assq a *macros*))
			 (if a (macroexpand (apply (cdr a) (cdr e))) e))
			(else e)))
      e))

(define gentemp
  (let ((*gensym-counter* -1))
    (lambda ()
      (set! *gensym-counter* (+ *gensym-counter* 1))
      (string->symbol
       (string-append "slib:G" (number->string *gensym-counter*))))))

;;;expand thoroughly, not just topmost expression.  While
;;;expanding subexpressions, the primitive forms quote, lambda,
;;;set!, let/*/rec, cond, case, do: need to be destructured
;;;properly.  (if, and, or, begin: don't need special treatment.)
(define (defmacro:macroexpand* e)
  (if (pair? e)
      (let* ((a (car e))
	     (c (and (symbol? a) (assq a *macros*))))
	(if c (defmacro:macroexpand* (apply (cdr c) (cdr e)))
	    (case a
	      ((quote) e)
	      ((lambda)
	       (cons a (cons (cadr e)
			     (map defmacro:macroexpand* (cddr e)))))
	      ((set!)
	       `(set! ,(cadr e)
		      ,(defmacro:macroexpand* (caddr e))))
	      ((let)
	       (let ((b (cadr e)))
		 (if (symbol? b)	;named let
		     `(let ,b
			,(map (lambda (vv)
				`(,(car vv)
				  ,(defmacro:macroexpand* (cadr vv))))
			      (caddr e))
			,@(map defmacro:macroexpand*
			       (cdddr e)))
		     `(let
			  ,(map (lambda (vv)
				  `(,(car vv)
				    ,(defmacro:macroexpand* (cadr vv))))
				b)
			,@(map defmacro:macroexpand*
			       (cddr e))))))
	      ((let* letrec)
	       `(,a ,(map (lambda (vv)
			    `(,(car vv)
			      ,(defmacro:macroexpand* (cadr vv))))
			  (cadr e))
		    ,@(map defmacro:macroexpand* (cddr e))))
	      ((cond)
	       `(cond
		 ,@(map (lambda (c)
			  (map defmacro:macroexpand* c))
			(cdr e))))
	      ((case)
	       `(case ,(defmacro:macroexpand* (cadr e))
		  ,@(map (lambda (c)
			   `(,(car c)
			     ,@(map defmacro:macroexpand* (cdr c))))
			 (cddr e))))
	      ((do)
	       `(do ,(map
		      (lambda (initsteps)
			`(,(car initsteps)
			  ,@(map defmacro:macroexpand*
				 (cdr initsteps))))
		      (cadr e))
		    ,(map defmacro:macroexpand* (caddr e))
		  ,@(map defmacro:macroexpand* (cdddr e))))
	      (else (map defmacro:macroexpand* e)))))
      e))

(require 'eval)
(define base:eval slib:eval)
(define (defmacro:eval x) (base:eval (defmacro:macroexpand* x)))
(define (defmacro:load <pathname>)
  (call-with-input-file <pathname>
    (lambda (port)
      (let ((old-load-pathname *load-pathname*))
	(set! *load-pathname* <pathname>)
	(do ((o (read port) (read port)))
	    ((eof-object? o))
	  (defmacro:eval o))
	(set! *load-pathname* old-load-pathname)))))

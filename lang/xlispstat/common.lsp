;;;;
;;;; Additional Common Lisp Functions for XLISP-STAT 2.0
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;

(setf (get 'first '*setf*)  #'(lambda (x y) (setf (nth 0 x) y)))
(setf (get 'second '*setf*) #'(lambda (x y) (setf (nth 1 x) y)))
(setf (get 'third '*setf*)  #'(lambda (x y) (setf (nth 2 x) y)))
(setf (get 'fourth '*setf*) #'(lambda (x y) (setf (nth 3 x) y)))

;;;;
;;;;
;;;; Defsetf and documentation functions
;;;;
;;;;

(defun apply-arg-rotate (f args) 
  (apply f (car (last args)) (butlast args)))

; (defsetf) - define setf method
(defmacro defsetf (sym first &rest rest)
"Syntax: (defsetf sym fcn [doc])
Installs #'FCN as setf method for SYM."
  (if (symbolp first)
      `(progn (setf (get ',sym '*setf*) #',first) ',sym)
      (let ((f `#'(lambda ,(append (car rest) first) ,@(cdr rest)))
            (args (gensym)))
        `(progn
          (setf (get ',sym '*setf*) 
                #'(lambda (&rest ,args) (apply-arg-rotate ,f ,args)))
          ',sym))))

;; (load-help) - read in file positions fo accessing help info.
(defun load-help ()
  (when 
   (and (null *help-loaded*) (streamp *help-stream*))
   (princ "loading in help file information - this will take a minute ...")
   (force-output)
   (setq *help-loaded* t)
   (file-position *help-stream* 0)
   (do ((item (read *help-stream* nil '*eof*) 
              (read *help-stream* nil '*eof*))) 
       ((eq item '*eof*))
       (cond
         ((and item
               (symbolp item) 
               (null (get item 'function-documentation)))
          (setf (get item 'function-documentation) 
                (file-position *help-stream*)))
         ((consp item)
         	(case (cadr item)
         	  (function (if (null (get (car item) 'function-documentation))
         		            (setf (get (car item) 'function-documentation) 
                                  (file-position *help-stream*))))
              (variable (if (null (get (car item) 'variable-documentation))
         		            (setf (get (car item) 'variable-documentation) 
                                  (file-position *help-stream*))))
              (type (if (null (get (car item) 'type-documentation))
         		        (setf (get (car item) 'type-documentation) 
                              (file-position *help-stream*))))
              (object
               (if (and (boundp (car item))
                        (objectp (symbol-value (car item)))
                        (null (send (symbol-value (car item))
                                    :internal-doc
                                    (caddr item))))
                   (send (symbol-value (car item))
                        :documentation
                        (caddr item)
                        (file-position *help-stream*))))))))
   (princ "done")
   (terpri)))

(defun documentation (symbol doc-type)
"Args: (symbol doc-type)
Returns SYMBOL documentation of type DOC-TYPE."
  (load-help)
  (let ((doc  (case doc-type
                (function (get symbol 'function-documentation))
                (variable (get symbol 'variable-documentation))
                (type     (get symbol 'type-documentation))
                (setf     (get symbol 'setf-documentation)))))
    (when (and (numberp doc) (streamp *help-stream*))
          (file-position *help-stream* doc)
          (setq doc (read *help-stream*)))
    doc))

(defsetf documentation (symbol doc-type) (value)
  (case doc-type
    (function (setf (get symbol 'function-documentation) value))
    (variable (setf (get symbol 'variable-documentation) value))
    (type     (setf (get symbol 'type-documentation) value))
    (setf     (setf (get symbol 'setf-documentation) value))))
  
;;;;
;;;;
;;;; Modules, provide and require
;;;;
;;;;

(defvar *modules*)
    
(defun provide (name)
"Args: (name)
Adds NAME to the list of modules."
  (pushnew name *modules* :test #'equal))
  
(defun require (name &optional (path name))
"Args: (name)
Loads module NAME, unless it has already been loaded. If PATH is supplied it
is used as the file name; otherwise NAME is used. If file NAME is not in the
current directory *default-path* is searched."
  (let ((name (string name))
        (path (string path)))
    (unless (member name *modules* :test #'equal)
            (if (load path)
                t
#+macintosh     (let ((vol (set-volume)))
                  (unwind-protect (load path)
                                  (set-volume vol)))
#-macintosh     (load (strcat *default-path* path))))))

;;;;
;;;;
;;;; Miscellaneous Functions
;;;;
;;;;

; setf method for select function
(defsetf select set-select)

(defun vectorp (x)
"Args: (m)
Returns T if M is a vector, NIL otherwise."
  (and (arrayp x) (= (array-rank x) 1)))

(defun matrixp (x)
"Args: (m)
Returns T if M is a matrix, NIL otherwise."
  (and (arrayp x) (= (array-rank x) 2)))

(defun equalp (x y)
"Args: (x y)
Returns T if (equal x y), or x, y are numbers and (= x y), or
x and y are strings and (string-equal x y)."
  (or (equal x y) 
      (and (numberp x) (numberp y) (= x y))
      (and (stringp x) (stringp y) (string-equal x y))))

(defun y-or-n-p (&rest args)
"Args: (&rest args)
Prints STRING, if provided,  and reads an answer until an answer of Y or N
is obtained. Returns T for Y, NIL for N."
  (do ((answer nil (read))) ((member answer '(y n)) (eq answer 'y))
    (if args (apply #'format t args))
    (princ " (Y/N)")))
    
(defmacro push (a l)
"Syntax: (push item place)
Pushes ITEM onto list in generalized variable PLACE."
  `(if ,l 
       (let ((temp ,l))
         (rplacd temp (cons (car ,l) (cdr ,l)))
         (rplaca temp ,a))
       (setf ,l (cons ,a NIL))))

(defmacro pushnew (a l &rest args)
"Syntax: (push item place &key :test :test-not)
Pushes ITEM onto PLACE if it is not already there."
  `(unless (member ,a ,l ,@args) (push ,a ,l) nil))

(defun getf (place indicator &optional default)
"Args: (place indicator &optional default)
Returns property value of INDICATOR in PLACE, or DEFAULT if not found."
  (let ((mem (member indicator place :test #'eq)))
    (if mem (second mem) default)))

(defun functionp (x)
"Args: (x)
Returns T if X is a legal argument to FUNCALL, NIL otherwise."
    (or (eq (type-of x) 'closure)
        (eq (type-of x) 'subr)
        (symbolp x)
        (and (consp x) (eq (car x) 'lambda))))

(defun count (x seq &key (test #'eql))
"Args (x seq &key (test #'eql))
Counts the number of times X occurs in SEQ, using TEST for matching."
  (reduce #'(lambda (sum y) (if (funcall test x y) (+ sum 1) sum)) 
          seq :initial-value 0))

(defmacro with-input-from-string (stream-string &rest body)
"Syntax: (with-input-from-string (stream string) {form}*)
Opens stream for reading from STRING, binds to STREAM and evaluates
FORMs with this binding."
  (let ((stream (first stream-string))
        (string (second stream-string)))
    `(let ((,stream (make-string-input-stream ,string)))
       (progn ,@body))))

(defmacro with-output-to-string (str-list &rest body)
"Syntax: (with-output-to-string (stream) {form}*)
Opens string output stream, binds to STREAM and evaluates FORMs with
this binding. Returns output stream string."
  (let ((stream (first str-list)))
    `(let ((,stream (make-string-output-stream)))
       (progn ,@body)
       (get-output-stream-string ,stream))))

(defmacro with-open-file (stream-file-args &rest body)
"Syntax: (with-open-file (stream filename {options}*) {form}*)
Opens file stream for FILENAME with specified options, binds to 
STREAM and evaluates FORMs with this binding. Closes stream regardless
of errors."
  (let ((stream (first stream-file-args))
	(file-args (rest stream-file-args)))
    `(let ((,stream (open ,@file-args)))
       (unwind-protect 
	   (progn ,@body)
	 (if ,stream (close ,stream))))))

(defun realp (x)
"Args: (x)
Returns true if X is a real number."
  (or (integerp x) (floatp x)))

;;;;
;;;;
;;;; Additional Common Lisp Functions for Xlisp 2.0
;;;; From the init.lsp file supplied in the Xlisp distribution
;;;;
;;;;

; (unintern sym) - remove a symbol from the oblist
(defun unintern (symbol)
"Args: (symbol)
Makes SYMBOL no longer present in *OBARRAY*.  Returns T if SYMBOL was present;
NIL otherwise."
  (let ((subhash (hash symbol (length *obarray*))))
    (cond ((member symbol (aref *obarray* subhash))
             (setf (aref *obarray* subhash)
                   (delete symbol (aref *obarray* subhash)))
             t)
          (t nil))))

; (mapcan fun list [ list ]...)
(defmacro mapcan (&rest args) `(apply #'nconc (mapcar ,@args)))

; (mapcon fun list [ list ]...)
(defmacro mapcon (&rest args) `(apply #'nconc (maplist ,@args)))

; (set-macro-character ch fun [ tflag ])
(defun set-macro-character (ch fun &optional tflag)
    (setf (aref *readtable* (char-int ch))
          (cons (if tflag :tmacro :nmacro) fun))
    t)

; (get-macro-character ch)
(defun get-macro-character (ch)
  (if (consp (aref *readtable* (char-int ch)))
    (cdr (aref *readtable* (char-int ch)))
    nil))

;;;;
;;;;
;;;; Additional System Functions for Xlisp 2.0
;;;; From the init.lsp file supplied in the Xlisp distribution
;;;;
;;;;

; (savefun fun) - save a function definition to a file
(defmacro savefun (fun)
"Args: (fun)
Safe function definition of symbol FUN to file FUN.lsp."
  `(let* ((fname (strcat (symbol-name ',fun) ".lsp"))
          (fval (get-lambda-expression (symbol-function ',fun)))
          (fp (open fname :direction :output)))
     (cond (fp (print (cons (if (eq (car fval) 'lambda)
                                'defun
                                'defmacro)
                            (cons ',fun (cdr fval))) fp)
               (close fp)
               fname)
           (t nil))))

; (debug) - enable debug breaks
(defun debug () 
"Args: ()
Enable breaking on error on."
  (setq *breakenable* t))

; (nodebug) - disable debug breaks
(defun nodebug ()
"Args: ()
Disable breaking on error on."
  (setq *breakenable* nil))

; (untrace) - patched to allow zero arguments
#+xlisp (setf (symbol-function '|untrace|) (symbol-function 'untrace))
#+xlisp (defmacro untrace (&rest args) `(|untrace| ,@(if args args (trace))))

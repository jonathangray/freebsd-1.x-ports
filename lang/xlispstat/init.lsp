; initialization file for XLISP-STAT 2.0

; get some more space
#+small-machine(expand 7)
#+(and macintosh (not small-machine))(expand 15)
#-macintosh(expand 20)

; load in lisp files
(load "common")

; initialize to disable breaks and trace back
(setq *breakenable* nil)
(setq *tracenable* nil)
#+small-machine (setq *keep-documentation-strings* nil)

#+macintosh (defvar *help-file-name* "xlisp.help")
#-macintosh (defvar *help-file-name* 
	            (format nil "~axlisp.help" *default-path*))
(defvar *help-stream* 
  (let ((file (open *help-file-name*)))
    (if file file (open (concatenate 'string *help-file-name* ".small")))))
(defvar *line-length* 78 "Line length used in printing help messages")
(defvar *keep-documentation-strings* t)
(defvar *help-loaded* nil)
(defconstant *cursors* 
  '(arrow watch cross brush hand finger hour-glass trash-bag trash-can))
(defconstant *colors* 
  '(white black red green blue cyan magenta yellow))
(defconstant *plot-symbols* 
  '(dot dot1 dot2 dot3 dot4 disk diamond cross square wedge1 wedge2 x))

; load xlispstat objects and related functions
(require "help")
(require "objects")

; load macintosh menus and editing functions
#+macintosh(progn
	     (setq *listener* (send listener-proto :new))
	     (require "menus")
	     (set-menu-bar *standard-menu-bar*))
#-macintosh(require "menubar")

; load statistics and graphics functions
(require "statistics")
(require "dialogs")
(require "graphics")
(require "regression")
(require "autoload.lsp")

; load user initialization file
(load "statinit")


;;;;
;;;; menus.lsp Menus for the Macintosh
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;

(provide "menus")

;;;;
;;;; Editing Methods
;;;;

(defmeth edit-window-proto :edit-selection ()
	(send (send edit-window-proto :new)
	      :paste-stream (send self :selection-stream)))

(defmeth edit-window-proto :eval-selection ()
  (let ((s (send self :selection-stream)))
    (do ((expr (read s nil '*eof*) (read s nil '*eof*)))
        ((eq expr '*eof*))
      (eval expr))))

(let ((last-string ""))
  (defmeth edit-window-proto :find ()
"Method args: ()
Opens dialog to get string to find and finds it. Beeps if not found."
    (let ((s (get-string-dialog "String to find:" :initial last-string)))
      (when s
          (if (stringp s) (setq last-string s))
          (unless (and (stringp s) (send self :find-string s))
                  (sysbeep)))))
  (defmeth edit-window-proto :find-again ()
    (unless (and (stringp last-string) 
                 (< 0 (length last-string))
                 (send self :find-string last-string))
            (sysbeep))))
                  
;;;;
;;;; General Menu Methods and Functions
;;;;
(defmeth menu-proto :find-item (str)
"Method args: (str)
Finds and returns menu item with tile STR."
  (dolist (item (send self :items))
    (if (string-equal str (send item :title)) (return item))))

(defun find-menu (title)
"Args: (title)
Finds and returns menu in the menu bar with title TITLE."
  (dolist (i *hardware-objects*)
          (let ((object (nth 2 i)))
            (if (and (kind-of-p object menu-proto) 
                     (send object :installed-p) 
                     (string-equal (string title) (send object :title)))
                (return object)))))

(defun set-menu-bar (menus)
"Args (menus)
Makes the list MENUS the current menu bar."
  (dolist (i *hardware-objects*)
          (let ((object (nth 2 i)))
            (if (kind-of-p object menu-proto) (send object :remove))))
  (dolist (i menus) (send i :allocate) (send i :install)))
  
;;;;
;;;; Apple Menu
;;;;
(defvar *apple-menu* (send apple-menu-proto :new (string #\apple)))
(send *apple-menu* :append-items 
  (send menu-item-proto :new "About XLISP-STAT" :action 'about-xlisp-stat)
  (send dash-item-proto :new))

;;;;
;;;; File Menu
;;;;
(defvar *file-menu* (send menu-proto :new "File"))

(defproto file-edit-item-proto '(message) '() menu-item-proto)

(defmeth file-edit-item-proto :isnew (title message &rest args)
  (setf (slot-value 'message) message)
  (apply #'call-next-method title args))
  
(defmeth file-edit-item-proto :do-action ()
  (send (front-window) (slot-value 'message)))
  
(defmeth file-edit-item-proto :update ()
  (send self :enabled (kind-of-p (front-window) edit-window-proto)))
  
(send *file-menu* :append-items 
  (send menu-item-proto :new "Load" :key #\L :action
    #'(lambda ()
      (let ((f (open-file-dialog t)))
        (when f (load f) (format t "; finished loading ~s~%" f)))))
  (send dash-item-proto :new)
  (send menu-item-proto :new "New Edit" :key #\N
        :action #'(lambda () (send edit-window-proto :new)))
  (send menu-item-proto :new "Open Edit" :key #\O
        :action #'(lambda () (send edit-window-proto :new :bind-to-file t)))
  (send dash-item-proto :new)
  (send file-edit-item-proto :new "Save Edit" :save :key #\S)
  (send file-edit-item-proto :new "Save Edit As" :save-as)
  (send file-edit-item-proto :new "Save Edit Copy" :save-copy)
  (send file-edit-item-proto :new "Revert Edit" :revert)
  (send dash-item-proto :new)
  (send menu-item-proto :new "Quit" :key #\Q :action 'exit))

;;;;
;;;; Edit Menu
;;;;
(defproto edit-menu-item-proto '(item message) '() menu-item-proto)

(defmeth edit-menu-item-proto :isnew (title item message &rest args)
  (setf (slot-value 'item) item)
  (setf (slot-value 'message) message)
  (apply #'call-next-method title args))
  
(defmeth edit-menu-item-proto :do-action ()
  (unless (system-edit (slot-value 'item))
          (let ((window (front-window)))
            (if window (send window (slot-value 'message))))))
          
(defvar *edit-menu* (send menu-proto :new "Edit"))
(send *edit-menu* :append-items
  (send edit-menu-item-proto :new "Undo" 0 :undo :enabled nil)
  (send dash-item-proto :new)
  (send edit-menu-item-proto :new "Cut" 2 :cut-to-clip :key #\X)
  (send edit-menu-item-proto :new "Copy" 3 :copy-to-clip :key #\C)
  (send edit-menu-item-proto :new "Paste" 4 :paste-from-clip :key #\V)
  (send edit-menu-item-proto :new "Clear" 5 :clear :enabled nil)
  (send dash-item-proto :new)
  (send menu-item-proto :new "Copy-Paste" :key #\/ :action
    #'(lambda () 
      (let ((window (front-window)))
        (when  window
              (send window :copy-to-clip)
              (send window :paste-from-clip)))))
  (send dash-item-proto :new)
  (send menu-item-proto :new "Find ..." :key #\F :action
    #'(lambda () 
      (let ((window (front-window))) 
        (if window (send window :find)))))
  (send menu-item-proto :new "Find Again" :key #\A :action
    #'(lambda () 
      (let ((window (front-window))) 
        (if window (send window :find-again)))))
  (send dash-item-proto :new)
  (send menu-item-proto :new "Edit Selection" :action
    #'(lambda () (send (front-window) :edit-selection)))
  (send menu-item-proto :new "Eval Selection" :key #\E :action
    #'(lambda () (send (front-window) :eval-selection))))

;;;;
;;;; Command Menu
;;;;
(defvar *command-menu* (send menu-proto :new "Command"))
(send *command-menu* :append-items
  (send menu-item-proto :new "Show XLISP-STAT"
                        :action #'(lambda () (send *listener* :show-window)))
  (send dash-item-proto :new)
  (send menu-item-proto :new "Clean Up" :key #\, :action #'clean-up)
  (send menu-item-proto :new "Toplevel" :key #\. :action #'top-level)
  (send dash-item-proto :new)
  (let ((item (send menu-item-proto :new "Dribble")))
    (send item :action 
        #'(lambda () 
            (cond
              ((send item :mark) (dribble) (send item :mark nil))
              (t (let ((f (set-file-dialog "Dribble file:")))
                   (when f
                         (dribble f)
                         (send item :mark t)))))))
    item))

(defconstant *standard-menu-bar* 
             (list *apple-menu* *file-menu* *edit-menu* *command-menu*))

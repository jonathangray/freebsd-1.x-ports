(require 'x-mouse)
(provide 'x-buffer-local-mouse)

; x-buffer-local-mouse
;
; Gene Rollins
; School of Computer Science
; Carnegie-Mellon University
; Pittsburgh, PA 15213
; rollins@cs.cmu.edu
;
; The package x-buffer-local-mouse allows one to easily make buffer-specific
; mouse bindings.  It can be loaded with:
;   (require 'x-buffer-local-mouse)
; 
; This package declares some buffer local variables to hold function symbols.
; There is one variable for each mouse/key combination.
; 
; No keys:                   x-left        x-middle        x-right
; Shift:                   x-s-left      x-s-middle      x-s-right
; Control:                 x-c-left      x-c-middle      x-c-right
; Control-Shift:         x-c-s-left    x-c-s-middle    x-c-s-right
; Meta:                    x-m-left      x-m-middle      x-m-right
; Meta-Shift:            x-m-s-left    x-m-s-middle    x-m-s-right
; Control-Meta:          x-c-m-left    x-c-m-middle    x-c-m-right
; Control-Meta-Shift:  x-c-m-s-left  x-c-m-s-middle  x-c-m-s-right
; 
; This is how to setup mouse bindings:
; Buffer local variables have a default value, that is used when a buffer
; doesn't have a buffer-local value for that variable.  Each buffer may have a
; local value for each variable listed above.  The default value is set with
; setq-default as follows:
;   (setq-default x-c-middle 'x-find-file)
; The local value is set with setq for the current buffer as in:
;   (setq x-c-middle 'x-find-tag)
; 
; This is useful in mode hook functions as in:
;   (defun lisp-mode-hook-function ()
;     (if (featurep 'x-buffer-local-mouse)
;         (progn
; 	    (setq x-c-middle 'x-find-tag)
;           (setq x-c-right  'x-find-tag-other))))
; 
; Default mouse bindings are given at the end of this file.
;
; Using the x-mouse package, one may make mouse bindings as follows:
;   (define-key mouse-map x-button-c-left 'x-mouse-select-this)
; The function convert-mouse-definitions will convert these to the buffer local
; style using setq-default. Set point to beginning of definitions before
; calling this function.  Optionally, before invoking, narrow region to just
; the definitions. Then type Meta-X convert-mouse-definitions.

(define-key mouse-map x-button-left       'x-left-caller)
(define-key mouse-map x-button-middle     'x-middle-caller)
(define-key mouse-map x-button-right      'x-right-caller)
(define-key mouse-map x-button-s-left     'x-s-left-caller)
(define-key mouse-map x-button-s-middle   'x-s-middle-caller)
(define-key mouse-map x-button-s-right    'x-s-right-caller)
(define-key mouse-map x-button-c-left     'x-c-left-caller)
(define-key mouse-map x-button-c-middle   'x-c-middle-caller)
(define-key mouse-map x-button-c-right    'x-c-right-caller)
(define-key mouse-map x-button-c-s-left   'x-c-s-left-caller)
(define-key mouse-map x-button-c-s-middle 'x-c-s-middle-caller)
(define-key mouse-map x-button-c-s-right  'x-c-s-right-caller)

(define-key mouse-map x-button-m-left       'x-m-left-caller)
(define-key mouse-map x-button-m-middle     'x-m-middle-caller)
(define-key mouse-map x-button-m-right      'x-m-right-caller)
(define-key mouse-map x-button-m-s-left     'x-m-s-left-caller)
(define-key mouse-map x-button-m-s-middle   'x-m-s-middle-caller)
(define-key mouse-map x-button-m-s-right    'x-m-s-right-caller)
(define-key mouse-map x-button-c-m-left     'x-c-m-left-caller)
(define-key mouse-map x-button-c-m-middle   'x-c-m-middle-caller)
(define-key mouse-map x-button-c-m-right    'x-c-m-right-caller)
(define-key mouse-map x-button-c-m-s-left   'x-c-m-s-left-caller)
(define-key mouse-map x-button-c-m-s-middle 'x-c-m-s-middle-caller)
(define-key mouse-map x-button-c-m-s-right  'x-c-m-s-right-caller)

(defvar x-left)       (make-variable-buffer-local 'x-left)
(defvar x-middle)     (make-variable-buffer-local 'x-middle)
(defvar x-right)      (make-variable-buffer-local 'x-right)
(defvar x-s-left)     (make-variable-buffer-local 'x-s-left)
(defvar x-s-middle)   (make-variable-buffer-local 'x-s-middle)
(defvar x-s-right)    (make-variable-buffer-local 'x-s-right)
(defvar x-c-left)     (make-variable-buffer-local 'x-c-left)
(defvar x-c-middle)   (make-variable-buffer-local 'x-c-middle)
(defvar x-c-right)    (make-variable-buffer-local 'x-c-right)
(defvar x-c-s-left)   (make-variable-buffer-local 'x-c-s-left)
(defvar x-c-s-middle) (make-variable-buffer-local 'x-c-s-middle)
(defvar x-c-s-right)  (make-variable-buffer-local 'x-c-s-right)

(defvar x-m-left)       (make-variable-buffer-local 'x-m-left)
(defvar x-m-middle)     (make-variable-buffer-local 'x-m-middle)
(defvar x-m-right)      (make-variable-buffer-local 'x-m-right)
(defvar x-m-s-left)     (make-variable-buffer-local 'x-m-s-left)
(defvar x-m-s-middle)   (make-variable-buffer-local 'x-m-s-middle)
(defvar x-m-s-right)    (make-variable-buffer-local 'x-m-s-right)
(defvar x-c-m-left)     (make-variable-buffer-local 'x-c-m-left)
(defvar x-c-m-middle)   (make-variable-buffer-local 'x-c-m-middle)
(defvar x-c-m-right)    (make-variable-buffer-local 'x-c-m-right)
(defvar x-c-m-s-left)   (make-variable-buffer-local 'x-c-m-s-left)
(defvar x-c-m-s-middle) (make-variable-buffer-local 'x-c-m-s-middle)
(defvar x-c-m-s-right)  (make-variable-buffer-local 'x-c-m-s-right)


(defun x-left-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-left)))
  (funcall fun-to-call arg))

(defun x-middle-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-middle)))
  (funcall fun-to-call arg))

(defun x-right-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-right)))
  (funcall fun-to-call arg))

(defun x-s-left-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-s-left)))
  (funcall fun-to-call arg))

(defun x-s-middle-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-s-middle)))
  (funcall fun-to-call arg))

(defun x-s-right-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-s-right)))
  (funcall fun-to-call arg))

(defun x-c-left-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-c-left)))
  (funcall fun-to-call arg))

(defun x-c-middle-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-c-middle)))
  (funcall fun-to-call arg))

(defun x-c-right-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-c-right)))
  (funcall fun-to-call arg))

(defun x-c-s-left-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-c-s-left)))
  (funcall fun-to-call arg))

(defun x-c-s-middle-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-c-s-middle)))
  (funcall fun-to-call arg))

(defun x-c-s-right-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-c-s-right)))
  (funcall fun-to-call arg))

(defun x-m-left-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-m-left)))
  (funcall fun-to-call arg))

(defun x-m-middle-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-m-middle)))
  (funcall fun-to-call arg))

(defun x-m-right-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-m-right)))
  (funcall fun-to-call arg))

(defun x-m-s-left-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-m-s-left)))
  (funcall fun-to-call arg))

(defun x-m-s-middle-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-m-s-middle)))
  (funcall fun-to-call arg))

(defun x-m-s-right-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-m-s-right)))
  (funcall fun-to-call arg))

(defun x-c-m-left-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-c-m-left)))
  (funcall fun-to-call arg))

(defun x-c-m-middle-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-c-m-middle)))
  (funcall fun-to-call arg))

(defun x-c-m-right-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-c-m-right)))
  (funcall fun-to-call arg))

(defun x-c-m-s-left-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-c-m-s-left)))
  (funcall fun-to-call arg))

(defun x-c-m-s-middle-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-c-m-s-middle)))
  (funcall fun-to-call arg))

(defun x-c-m-s-right-caller (arg)
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (setq fun-to-call x-c-m-s-right)))
  (funcall fun-to-call arg))


(defun x-mouse-line-to-top-of-window (arg)
  "Relocate to the top of the window the line at the mouse location."
  (x-mouse-set-point arg)
  (recenter 0))

(defun x-mouse-line-to-bottom-of-window (arg)
  "Relocate to the bottom of the window the line at the mouse location."
  (x-mouse-set-point arg)
  (recenter (- (window-height) 2)))

(defun x-mouse-top-of-window-to-line (arg)
  "Take the line at the top of the window and relocate to the mouse location."
  (x-mouse-set-point arg)
  (let ((screen-y (car (cdr arg)))
	(window-top (car (cdr (window-edges)))))
    (x-mouse-set-point (cons (car arg) (cons window-top nil)))
    (scroll-down (- screen-y window-top)))
  )	       

(defun x-yank (arg)
  "Move point to mouse position and insert contents of killbuffer."
  (x-mouse-set-point arg)
  (yank))

(defun x-mouse-list-buffers (arg)
 "Pop up a list of buffers in another window."
  (list-buffers nil))

(defun x-execute-keyboard-macro (arg)
  "Execute current keyboard macro starting at mouse location."
  (x-mouse-set-point arg)
  (call-last-kbd-macro))

(defvar pathname-pattern-forward   "[~/A-Za-z0-9---_.]+")
(defvar pathname-pattern-backward "[^~/A-Za-z0-9---_.]+")

(defun match-pathname ()
  "Returns the string of an existing filename or causes an error."
  (save-excursion
    (re-search-backward pathname-pattern-backward)
    (re-search-forward pathname-pattern-forward)
    (let ((filestring (buffer-substring (match-beginning 0) (match-end 0))))
      (if (file-exists-p filestring)
	  filestring
	  (error "File %s does not exist." filestring)))))

(defun copy-pathname ()
  "Matches a pathname around mouse location; copies it into kill & X buffers."
  (re-search-backward pathname-pattern-backward)
  (re-search-forward pathname-pattern-forward)
  (let ((beg (match-beginning 0)) (end (match-end 0)))
    (copy-region-as-kill beg end)
    (x-store-cut-buffer (buffer-substring beg end))))

(defun x-find-file (arg)
  "Match a pathname around mouse location.  Find the file named by the
pathname in this window.  If in the buffer menu, select this line's
buffer in this window."
  (x-mouse-set-point arg)
  (if (eq major-mode 'Buffer-menu-mode)
      (Buffer-menu-this-window)
      (find-file (match-pathname))))

(defun x-find-file-other (arg)
  "Match a pathname around mouse location.  Find the file named by the
pathname in another window.  If in the buffer menu, select this line's
buffer in another window."
  (x-mouse-set-point arg)
  (if (eq major-mode 'Buffer-menu-mode)
      (Buffer-menu-other-window)
      (find-file-other-window (match-pathname))))

(defun x-copy-pathname (arg)
  "Match a pathname around the mouse location, stuff it in the killbuffer and
the X cut buffer.  Does not set point."
  (save-window-excursion
    (save-excursion
      (x-mouse-set-point arg)
      (copy-pathname))))

(defvar x-desc nil)

(defun x-describe-mouse-keys (arg)
  "Describe the current bindings of the mouse keys in the window pointed at."
  (if arg (x-mouse-set-point arg))
  (setq x-desc (concat "(In Major Mode: " mode-name ")\n"))
  (describe-mouse-key 'x-left)
  (describe-mouse-key 'x-middle)
  (describe-mouse-key 'x-right)
  (describe-mouse-key 'x-s-left)
  (describe-mouse-key 'x-s-middle)
  (describe-mouse-key 'x-s-right)
  (describe-mouse-key 'x-c-left)
  (describe-mouse-key 'x-c-middle)
  (describe-mouse-key 'x-c-right)
  (describe-mouse-key 'x-c-s-left)
  (describe-mouse-key 'x-c-s-middle)
  (describe-mouse-key 'x-c-s-right)
  (describe-mouse-key 'x-m-left)
  (describe-mouse-key 'x-m-middle)
  (describe-mouse-key 'x-m-right)
  (describe-mouse-key 'x-m-s-left)
  (describe-mouse-key 'x-m-s-middle)
  (describe-mouse-key 'x-m-s-right)
  (describe-mouse-key 'x-c-m-left)
  (describe-mouse-key 'x-c-m-middle)
  (describe-mouse-key 'x-c-m-right)
  (describe-mouse-key 'x-c-m-s-left)
  (describe-mouse-key 'x-c-m-s-middle)
  (describe-mouse-key 'x-c-m-s-right)
  (pop-to-buffer "*X-Help*")
  (delete-region (point-min) (point-max))
  (insert x-desc)
  (goto-char (point-min)))

(defun describe-mouse-keys ()
  (interactive)
  (x-describe-mouse-keys nil))

(defun describe-mouse-key (key)
  (let ((val (symbol-value key)))
    (setq x-desc
	  (concat x-desc "* " (symbol-name key) ": " (symbol-name val) "\n "
		  (documentation val) "\n"))))

(defun convert-mouse-definitions ()
  "Convert regular mouse binding definitions to buffer-local style.
Set point to beginning of definitions before calling this function.
Optionally, before invoking, narrow region to just the definitions."
  (interactive)
  (let ((beg (point)))
    (replace-string "x-button-" "x-")
    (goto-char beg)
    (replace-string "define-key mouse-map" "setq-default")
    (goto-char beg)
    (replace-string "x-mouse-select-this" "x-find-file")
    (goto-char beg)
    (replace-string "x-mouse-select-other" "x-find-file-other")
    (goto-char beg)))

;;; These are the default mouse bindings.  You can make your own defaults
;;; by copying these and changing the function values.

(setq-default x-left 'x-mouse-set-point)
(setq-default x-middle 'x-mouse-line-to-top-of-window)
(setq-default x-right 'x-mouse-line-to-bottom-of-window)

(setq-default x-s-left 'x-cut-text)
(setq-default x-s-middle 'x-paste-text)
(setq-default x-s-right 'x-cut-and-wipe-text)

(setq-default x-c-left 'x-copy-pathname)
(setq-default x-c-middle 'x-find-file)
(setq-default x-c-right 'x-find-file-other)

(setq-default x-c-s-left 'x-execute-keyboard-macro)
(setq-default x-c-s-middle 'x-yank)
(setq-default x-c-s-right 'x-describe-mouse-keys)

;;; Some of the following meta combinations are probably captured by the
;;; window manager, and are not accessible to emacs.

(setq-default x-m-left 'x-mouse-list-buffers)
(setq-default x-m-middle 'x-mouse-list-buffers)
(setq-default x-m-right 'x-mouse-list-buffers)

(setq-default x-m-s-left 'x-mouse-set-mark)
(setq-default x-m-s-middle 'x-mouse-top-of-window-to-line)
(setq-default x-m-s-right 'x-mouse-keep-one-window)

(setq-default x-c-m-left 'x-mouse-list-buffers)
(setq-default x-c-m-middle 'x-mouse-list-buffers)
(setq-default x-c-m-right 'x-mouse-list-buffers)

(setq-default x-c-m-s-left 'x-help)
(setq-default x-c-m-s-middle 'x-yank)
(setq-default x-c-m-s-right 'x-describe-mouse-keys)

(require 'tags)
(provide 'mouse-tags)

(defvar lisp-tag-forward   "[~/A-Za-z0-9---.=_%$]+")
(defvar lisp-tag-backward "[^~/A-Za-z0-9---.=_%$]+")
(defvar sml-tag-forward   "[/A-Za-z0-9---_%$]+")
(defvar sml-tag-backward "[^/A-Za-z0-9---_%$]+")

(defvar *tag-pattern-forward* sml-tag-forward)
(defvar *tag-pattern-backward* sml-tag-backward)

(make-variable-buffer-local '*tag-pattern-forward*)
(make-variable-buffer-local '*tag-pattern-backward*)

(defun match-tag ()
  "Find the beginning and the end points of a tag (identifier) around point.
Unless the buffer is the Tags List, in which case match the whole line."
  (if (eq (current-buffer) (get-buffer "*Tags List*"))
      (progn
	(let (bol eol)
	  (beginning-of-line)
	  (setq bol (point))
	  (end-of-line)
	  (setq eol (point))
	  (buffer-substring bol eol)))
      (progn
	(re-search-backward *tag-pattern-backward*)
	(re-search-forward *tag-pattern-forward*)
	(buffer-substring (match-beginning 0) (match-end 0)))))

(defun x-find-tag (arg)
  "Match an identifier around the mouse position and do find-tag on it.
Uses the buffer-local variables *tag-pattern-forward* and
*tag-pattern-backward* to scan backward and forward to match the
identifier.  These default to patterns for matching SML identifiers."
  (x-mouse-set-point arg)
  (find-tag (match-tag) nil))

(defun x-find-tag-other (arg)
  "Match an identifier & do a find-tag-other-window on it.
See x-mouse-find-tag for details."
  (x-mouse-set-point arg)
  (find-tag-other-window (match-tag) nil))

;; ml-forms.el. Code to insert common forms in ML.
;; Hacked by Olin Shivers from Lars Bo Nielsen's sml.el.

;; YOUR .EMACS FILE
;;=============================================================================
;; This package should be loaded in by ml.el's load hook:
;; (setq ml-load-hook '((lambda () (require 'ml-forms))))

(provide 'ml-forms)

;; Install the keybindings on the ml-map and 
(define-key          ml-mode-map "\C-c\C-m" 'ml-insert-form)
(define-key inferior-ml-mode-map "\C-c\C-m" 'ml-insert-form)

(defconst ml-form-alist
  '(("let") ("local") ("signature") ("structure") ("datatype")
    ("case") ("functor") ("abstype") ("abstraction"))
  "The list of regions to auto-insert.")

(defun ml-insert-form ()
  "Interactive short-cut. Insert a common ML form."
  (interactive)
  (let ((newline nil)			; Did we insert a newline
	(name (completing-read "Form to insert: (default let) "
			       ml-form-alist nil t nil)))
    ;; default is "let"
    (if (string= name "") (setq name "let"))
    ;; Insert a newline if point is not at empty line
    (ml-indent-line)			; Indent the current line
    (if (save-excursion (beginning-of-line) (skip-chars-forward "\t ") (eolp))
	()
      (setq newline t)
      (insert "\n"))
    (condition-case ()
	(cond
	 ((string= name "let") (ml-let))
	 ((string= name "local") (ml-local))
	 ((string= name "structure") (ml-structure))
	 ((string= name "signature") (ml-signature))
	 ((string= name "abstraction") (ml-abstraction))
	 ((string= name "functor") (ml-functor))
	 ((string= name "case") (ml-case))
	 ((string= name "abstype") (ml-abstype))
	 ((string= name "datatype") (ml-datatype)))
      (quit (if newline 
		(progn
		  (delete-char -1)
		  (beep)))))))

(defun ml-let () 
  "Insert a `let in end'."
  (interactive) (ml-let-local "let"))

(defun ml-local ()
  "Insert a `local in end'."
  (interactive) (ml-let-local "local"))

(defun ml-signature ()
  "Insert a `signature ??? = sig end', prompting for name."
  (interactive) (ml-structure-signature "signature"))

(defun ml-structure ()
  "Insert a `structure ??? = struct end', prompting for name."
  (interactive) (ml-structure-signature "structure"))

(defun ml-case ()
  "Insert a case, prompting for case-expresion."
  (interactive)
  (let (indent (expr (read-string "Case expr: ")))
    (insert (concat "case " expr))
    (ml-indent-line)
    (setq indent (current-indentation))
    (end-of-line)
    (if ml-case-indent
	(progn
	  (insert "\n")
	  (indent-to (+ 2 indent))
	  (insert "of "))
      (insert " of\n")
      (indent-to (+ indent ml-indent-level)))
    (save-excursion (insert " => "))))

(defun ml-let-local (starter)
  (let (indent)
    (insert starter)
    (ml-indent-line)
    (setq indent (current-indentation))
    (end-of-line)
    (insert "\n") (indent-to (+ ml-indent-level indent))
    (insert "\n") (indent-to indent)
    (insert "in\n") (indent-to (+ ml-indent-level indent))
    (insert "\n") (indent-to indent)
    (insert "end") (previous-line 3) (end-of-line)))

(defun ml-structure-signature (which)
  (let (indent
	(name (read-string (concat "Name of " which ": "))))
    (insert (concat which " " name " ="))
    (ml-indent-line)
    (setq indent (current-indentation))
    (end-of-line)
    (insert "\n") (indent-to (+ ml-indent-level indent))
    (insert (if (string= which "signature") "sig\n" "struct\n"))
    (indent-to (+ (* 2 ml-indent-level) indent))
    (insert "\n") (indent-to (+ ml-indent-level indent))
    (insert "end") (previous-line 1) (end-of-line)))

(defun ml-functor ()
  "Insert a `funtor ??? () : ??? = struct end', prompting for name and type."
  (let (indent
	(name (read-string "Name of functor: "))
	(signame (read-string "Signature type of functor: ")))
    (insert (concat "functor " name " () : " signame " ="))
    (ml-indent-line)
    (setq indent (current-indentation))
    (end-of-line)
    (insert "\n") (indent-to (+ ml-indent-level indent))
    (insert "struct\n")
    (indent-to (+ (* 2 ml-indent-level) indent))
    (insert "\n") (indent-to (+ ml-indent-level indent))
    (insert "end") (previous-line 1) (end-of-line)))

(defun ml-abstraction ()
  "Insert a `abstraction ??? : ??? = struct end', prompting for name and type."
  (let (indent
	(name (read-string "Name of abstraction: "))
	(signame (read-string "Signature type of abstraction: ")))
    (insert (concat "abstraction " name " : " signame " ="))
    (ml-indent-line)
    (setq indent (current-indentation))
    (end-of-line)
    (insert "\n") (indent-to (+ ml-indent-level indent))
    (insert "struct\n")
    (indent-to (+ (* 2 ml-indent-level) indent))
    (insert "\n") (indent-to (+ ml-indent-level indent))
    (insert "end") (previous-line 1) (end-of-line)))

(defun ml-datatype ()
  "Insert a `datatype ??? =', prompting for name."
  (let (indent 
	(type (read-string (concat "Type of datatype (default none): ")))
	(name (read-string (concat "Name of datatype: "))))
    (insert (concat "datatype "
		    (if (string= type "") "" (concat type " "))
		    name " ="))
    (ml-indent-line)
    (setq indent (current-indentation))
    (end-of-line) (insert "\n") (indent-to (+ ml-indent-level indent))))

(defun ml-abstype ()
  "Insert an `abstype 'a ??? = with ... end'"
  (let (indent
	(typevar (read-string "Name of typevariable (default 'a): "))
	(type (read-string "Name of abstype: ")))
    (if (string= typevar "")
	(setq typevar "'a"))
    (insert (concat "abstype " typevar " " type " ="))
    (ml-indent-line)
    (setq indent (current-indentation))
    (insert "\n") (indent-to (+ ml-indent-level indent))
    (insert "\n") (indent-to indent)
    (insert "with\n") (indent-to (+ ml-indent-level indent))
    (insert "\n") (indent-to indent)
    (insert "end")
    (previous-line 3)
    (end-of-line)))

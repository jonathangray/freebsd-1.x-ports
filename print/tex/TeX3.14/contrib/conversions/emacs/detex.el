;;; Detex
;;; Bengt Martensson
;;; LastEditDate="Sun Oct  4 17:54:06 1987"
     
;;; For redistribution the same rules apply as for GNU Emacs.
;;; These rules are the following (quoted from a GNU Emacs .el file)

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(defvar detex-tex-commands-with-garbage-argument
  '("input" "special" "message")
  "List of TeX-commands for which their argument should be filtered out
by detex.")

(defvar detex-latex-commands-with-garbage-argument
  (append detex-tex-commands-with-garbage-argument
	  '("documentstyle" "pagestyle" "thispagestyle" 
	    "pagenumbering" "typeout" "typein" "label" "ref" "pageref"
	    "bibliography" "bibitem" "bibliographystyle"
	    "cite" "include" "includeonly" "begin" "end"))
    "List of LaTeX-commands for which their argument should be filtered out
by delatex.")

(defvar detex-latex-garbage-environments
  '("array" "equation" "eqnarray" "eqnarray*" "verbatim" "displaymath")
  "List of LaTeX environments which will be deleted by delatex.")

(defvar detex-tex-escape "" "TeX escape character used by detex.")
(defvar detex-re-tex-escape ""
  "Regular expression for TeX escape character.  Used by detex.")
(defvar detex-not-tex-escape ""
  "Regular expression that matches anything but TeX escape character.
Used by detex.")
(defvar detex-latex-optional-left ""
  "LaTeX character denoting left bracket for optional argument.
Used by detex.")
(defvar detex-tex-left "" "TeX left group character used by detex.")
(defvar detex-tex-right "" "TeX right group character used by detex.")

(make-variable-buffer-local 'detex-tex-escape)
(make-variable-buffer-local 'detex-re-tex-escape)
(make-variable-buffer-local 'detex-not-tex-escape)
(make-variable-buffer-local 'detex-latex-optional-left)
(make-variable-buffer-local 'detex-tex-left)
(make-variable-buffer-local 'detex-tex-right)

(set-default 'detex-tex-escape "\\")
(set-default 'detex-re-tex-escape "\\\\")
(set-default 'detex-not-escape "[^\\\\]")
(set-default 'detex-latex-optional-left "\\[")
(set-default 'detex-tex-left "{")
(set-default 'detex-tex-right "}")

(defun detex-remove-latex-commands-with-garbage-argument ()
  "Deletes latex-commands in detex-latex-commands-with-garbage-argument
together with their arguments."
  ;;(message "detex-remove-latex-commands-with-garbage-argument...")
  (mapcar 'detex-remove-latex-command-with-garbage-argument
	  detex-latex-commands-with-garbage-argument))

(defun detex-remove-tex-commands-with-garbage-argument ()
  "Deletes tex-commands in detex-tex-commands-with-garbage-argument
together with their arguments."
  (mapcar 'detex-remove-tex-command-with-garbage-argument
	  detex-tex-commands-with-garbage-argument)) 

(defun detex-remove-latex-command-with-garbage-argument (com)
"Deletes latex-command COM together with its arguments."
  (let ((command (concat detex-tex-escape com))
	(re-command (concat detex-re-tex-escape com "\\b"))
	b)
    (goto-char (point-min))
    (while (re-search-forward re-command (point-max) t)
      (search-backward command)
      (setq b (point))
      (search-forward command)
      (re-search-forward "[ \t]*")
      (if (looking-at detex-latex-optional-left)
	  (forward-sexp 1))
      (re-search-forward "[ \t]*")
      (if (looking-at detex-tex-left)
	  (forward-sexp 1)
	(forward-word 1))
      (delete-region b (point)))))

(defun detex-remove-tex-command-with-garbage-argument (com)
"Deletes tex-command COM together with its arguments."
  (let ((command (concat detex-tex-escape com))
	(re-command (concat detex-re-tex-escape com "\\b"))
	b)
    (goto-char (point-min))
    (while (re-search-forward re-command (point-max) t)
      (search-backward command)
      (setq b (point))
      (search-forward command)
      (re-search-forward "[ \t]*")
      (if (looking-at detex-tex-left)
	  (forward-sexp 1)
	(forward-word 1))
      (delete-region b (point)))))

(defun detex-remove-latex-garbage-environments ()
  "Deletes the latex environments in detex-latex-garbage-environments."
  ;;(message "detex-remove-latex-garbage-environments")
  (mapcar 'detex-remove-latex-environment detex-latex-garbage-environments))

(defun detex-remove-latex-environment (env)
  "Deletes latex environment ENV."
  (goto-char (point-min))
  (while (search-forward
	  (concat detex-tex-escape "begin" detex-tex-left env) (point-max) t)
    (search-backward detex-tex-escape)
    (setq b (point))
    (if (search-forward
	 (concat detex-tex-escape "end" detex-tex-left env) (point-max) t)
	(if (looking-at detex-tex-right)
	    (delete-region b (1+ (point)))))))
  
(defun detex-remove-discretionary-hyphens ()
  "Removes \\-."
  ;;(message "detex-remove-hyphens...")
  (goto-char (point-min))
  (while (re-search-forward (concat detex-re-tex-escape "-") (point-max) t)
    (replace-match "")))

(defun detex-remove-display (&optional flavor)
  "Deletes displays delimited by ""$$"".  If optional argument FLAVOR is 
""latex"", then it also recognizes LaTeX displays."
  (let (beg)
    (goto-char (point-min))
    ;;(message "detex-remove-display...")
    (while (re-search-forward (concat detex-not-escape "\\$\\$") (point-max) t)
      (setq beg (- (point) 2))
      (if (re-search-forward (concat detex-not-escape "\\$\\$") (point-max) t)
	  (delete-region beg (point))))
    (if (equal flavor "latex")
	(progn
	  (goto-char (point-min))
	  (while (search-forward (concat detex-tex-escape "[") (point-max) t)
	    (setq beg (- (point) 2))
	    (if (search-forward (concat detex-tex-escape "]") (point-max) t)
		(delete-region beg (point))))))))

(defun detex-remove-math (&optional flavor)
  "Deletes math mode.  If optional argument FLAVOR is ""latex"", it also
recognizes LaTeX constructions."
  (let (beg)
    ;;(message "detex-remove-math...")
    (goto-char (point-min))
    (while (re-search-forward (concat detex-not-escape "\\$") (point-max) t)
      (setq beg (1- (point)))
      (if (re-search-forward (concat detex-not-escape "\\$") (point-max) t)
	  (delete-region beg (point))))
    (if (equal flavor "latex")
	(progn
	  (goto-char (point-min))
	  (while (search-forward (concat detex-tex-escape "(") (point-max) t)
	    (setq beg (- (point) 2))
	    (if (search-forward (concat detex-tex-escape ")") (point-max) t)
		(delete-region beg (point))))))))

(defun detex-remove-tex-commands ()
  ;;(message "detex-remove-tex-commands")
  (goto-char (point-min))
  (while (re-search-forward
	  (concat detex-re-tex-escape "[a-zA-Z]*") (point-max) t)
    (replace-match "")))
     
(defun detex-remove-comments ()
  ;;(message "detex-remove-comments...")
  (goto-char (point-min))
  (while (re-search-forward (concat detex-not-escape "%.*$") (point-max) t)
    (delete-region (1+ (match-beginning 0)) (match-end 0))))
 
(defun detex-remove-font ()
  (goto-char (point-min))
  (while (re-search-forward (concat detex-re-tex-escape "font[^=]* *= *\\w*")
			    (point-max) t)
    (replace-match "")))

(defun detex-remove-double-single-quotes ()
  (goto-char (point-min))
  (replace-string "''" "" nil))
			    
;;; If using a TeX/LaTeX dialect which uses other characters for 
;;; escape and grouping, this function should be rewritten to
;;; redefine TeX escape character etc, possibly mode-dependent.

(defun detex-regler ()
  (interactive)
  (setq detex-tex-escape "!")
  (setq detex-re-tex-escape "!")
  (setq detex-not-escape "[^!]")
  (setq detex-tex-left "<")
  (setq detex-tex-right ">")
  (detex))
  
(defun delatex ()
  (interactive)
  ;;(define-escape-and-group)
  (detex-remove-latex-garbage-environments)
  (detex-remove-display "latex")
  (detex-remove-math "latex")
  (detex-remove-comments)
  (detex-remove-latex-commands-with-garbage-argument)
  (detex-remove-discretionary-hyphens)
  (detex-remove-double-single-quotes)
  (detex-remove-tex-commands))

(defun detex ()
  (interactive)
  ;;(define-escape-and-group)
  (detex-remove-display "tex")
  (detex-remove-math "tex")
  (detex-remove-comments)
  (detex-remove-font)
  (detex-remove-tex-commands-with-garbage-argument)
  (detex-remove-discretionary-hyphens)
  (detex-remove-double-single-quotes)
  (detex-remove-tex-commands))


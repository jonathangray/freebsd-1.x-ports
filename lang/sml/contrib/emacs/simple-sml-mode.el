(provide 'simple-sml-mode)

(defun back-to-outer-indent ()
  "Unindents out to the next outer level of indentation. Bound to \e\t"
  (interactive)
  (let ((start-column (current-column))
	(indent (current-column)))
    (if (> start-column 0)
	(progn
	  (save-excursion
	    (while (>= indent start-column)
	      (if (re-search-backward "^[^\n]" nil t)
		  (setq indent (current-indentation))
		(setq indent 0))))
	  (backward-delete-char-untabify (- start-column indent))))))

(defvar simple-sml-mode-map ())

(if simple-sml-mode-map
    ()
  (setq simple-sml-mode-map (make-sparse-keymap))
  (define-key simple-sml-mode-map "\e\t" 'back-to-outer-indent)
  (define-key simple-sml-mode-map "\t" 'indent-relative))

(defun simple-sml-mode ()
  "Major mode for editing SML source code.  Turning on simple-sml-mode
calls the value of the variable simple-sml-mode-hook, if that value is
non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map text-mode-map)
  (define-abbrev-table 'text-mode-abbrev-table ())
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-relative-maybe)
  (setq indent-tabs-mode nil)
  (use-local-map simple-sml-mode-map)
  (setq mode-name "Simple-SML")
  (setq major-mode 'simple-sml-mode)
  (run-hooks 'simple-sml-mode-hook))

(defun default-simple-sml-mode-hook-function ()
  (if (featurep 'x-buffer-local-mouse)
      (progn
	(setq x-c-middle 'x-find-tag)
	(setq x-c-right  'x-find-tag-other))
      (if (featurep 'fancy-xmouse)
	  (progn
	    (define-local-mouse-button c-s-middle 'x-find-tag)
	    (define-local-mouse-button c-s-right  'x-find-tag-other)))))

(defvar simple-sml-mode-hook 'default-simple-sml-mode-hook-function)



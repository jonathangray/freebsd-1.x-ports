(require 'tags)
(provide 'sml-tags)

(defun next-tag () 
  "Find next alternate definition of last tag specified."
  (interactive) (find-tag last-tag t))

(defun sml-toggle-case-fold ()
  "Toggle case-fold-search for doing sml tag searches."
  (interactive)
  (set-buffer (get-file-buffer tags-file-name))
  (setq case-fold-search (not case-fold-search))
  (message "%s" case-fold-search))

(defun new-tags-apropos (string)
  "Display list of all tags in tag table REGEXP matches."
  (interactive "sTag apropos (regexp): ")
  (with-output-to-temp-buffer "*Tags List*"
    (princ "Tags matching regexp ")
    (prin1 string)
    (terpri)
    (save-excursion
     (set-buffer (get-buffer "*Tags List*"))
     (setq major-mode 'simple-sml-mode)
     (run-hooks 'simple-sml-mode-hook)
     (visit-tags-table-buffer)
     (goto-char 1)
     (while (re-search-forward string nil t)
       (beginning-of-line)
       (princ (buffer-substring (point)
				(progn (skip-chars-forward "^\177")
				       (point))))
       (terpri)
       (forward-line 1)))))

(defun sml-tags-apropos (regexp)
  "Do tags-apropos for SML with case-fold-search = t; reset case-fold-search."
  (interactive "sSML Tag apropos (regexp): ")
  (let ((this-buf (current-buffer))
	(tags-buffer (get-file-buffer tags-file-name))
	case-fold)
    (set-buffer tags-buffer)
    (setq case-fold case-fold-search)
    (setq case-fold-search t)
    (new-tags-apropos regexp)
    (set-buffer tags-buffer)
    (setq case-fold-search case-fold)))

(defun sml-visit-tags-table (file)
  "Visit tags table, and set case-fold-search = nil for the tags buffer."
  (interactive (list (read-file-name "Visit SML tags table: (default TAGS) "
				     default-directory
				     (concat default-directory "TAGS")
				     t)))
  (visit-tags-table file)
  (set-buffer (find-file-noselect tags-file-name))
  (setq case-fold-search nil))

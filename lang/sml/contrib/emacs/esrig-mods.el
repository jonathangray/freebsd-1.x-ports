;;; sml-mods.el -- modifications to LBN's sml-mode, V. 3, May 1990

;;; Bruce Esrig  25 July 1991

;; (defvar sml-mode-map nil "The mode map used in sml-mode.")
;; (if sml-mode-map
;;    ()
  (setq sml-mode-map (make-sparse-keymap))
  (define-key sml-mode-map "\C-c'" 'sml-next-error)
  (define-key sml-mode-map "\C-c\C-v" 'sml-mode-version)
  (define-key sml-mode-map "\C-c\C-u" 'sml-save-buffer-use-file)
  (define-key sml-mode-map "\C-c\C-s" 'sml-pop-to-shell)
  (define-key sml-mode-map "\C-c\C-r" 'sml-send-region)
  (define-key sml-mode-map "\C-c\C-m" 'sml-region)
  (define-key sml-mode-map "\C-c\C-k" 'sml-skip-errors)
  (define-key sml-mode-map "\C-c\C-f" 'sml-run-on-file)
  (define-key sml-mode-map "\C-c\C-c" 'sml-send-function)
  (define-key sml-mode-map "\C-c\C-b" 'sml-send-buffer)
  (define-key sml-mode-map "\C-ci" 'sml-import-file)
  (define-key sml-mode-map "\e|" 'sml-electric-pipe)
  (define-key sml-mode-map "\e\t" 'sml-back-to-outer-indent)

  ;; Since return is not mapped to C-j, use C-m here instead
  (define-key sml-mode-map "\C-m" 'reindent-then-newline-and-indent)
  (define-key sml-mode-map "\177" 'backward-delete-char-untabify)
  (define-key sml-mode-map "\;" 'sml-electric-semi)
  (define-key sml-mode-map "\C-c\t" 'sml-indent-region)
  (define-key sml-mode-map "\t" 'sml-indent-line)

(defun sml-structure-signature (which)
  (let (indent
	(name (read-string (concat "Name of " which ": ")))
	sig)
    (if (string= which "structure")
	(setq sig (read-string (concat "Name of signature: "))))
    (if (string= which "structure")
	(insert (concat which " " name " (* : " sig " *) ="))
      (insert (concat which " " name " =")))
    (sml-indent-line)
    (setq indent (current-indentation))
    (end-of-line)
    (insert "\n") (indent-to (+ sml-indent-level indent))
    (insert (if (string= which "signature") "sig\n" "struct\n"))
    (indent-to (+ (* 2 sml-indent-level) indent))
    (insert "\n") (indent-to (+ sml-indent-level indent))
    (insert "end") (previous-line 1) (end-of-line)))

;; add include to these two vars
(defconst sml-indent-starters-reg
  "abstraction\\b\\|abstype\\b\\|and\\b\\|case\\b\\|datatype\\b\
\\|else\\b\\|fun\\b\\|functor\\b\\|if\\b\\|sharing\\b\
\\|in\\b\\|include\\b\\|infix\\b\\|infixr\\b\\|let\\b\\|local\\b\
\\|nonfix\\b\\|of\\b\\|open\\b\\|raise\\b\\|sig\\b\\|signature\\b\
\\|struct\\b\\|structure\\b\\|then\\b\\|\\btype\\b\\|val\\b\
\\|while\\b\\|with\\b\\|withtype\\b"
  "The indentation starters. The next line, after one starting with
one of these, will be indented.")

(defconst sml-starters-reg
  "\\babstraction\\b\\|\\babstype\\b\\|\\bdatatype\\b\
\\|\\bexception\\b\\|\\bfun\\b\\|\\bfunctor\\b\\|\\blocal\\b\
\\|\\binclude\\b\\|\\binfix\\b\\|\\binfixr\\b\\|\\bsharing\\b\
\\|\\bnonfix\\b\\|\\bopen\\b\\|\\bsignature\\b\\|\\bstructure\\b\
\\|\\btype\\b\\|\\bval\\b\\|\\bwithtype\\b\\|\\bwith\\b"
  "The starters of new expressions.")

;;; Added datatype as a kind of "function definition" near end of this fn
;;; Fixed the first line of a match expression to have the same indent
;;;   as the subsequent lines.
(defun sml-calculate-indentation ()
  (save-excursion
    (let ((case-fold-search nil))
      (beginning-of-line)
      (if (bobp)			; Beginning of buffer
	  0				; Indentation = 0
	(skip-chars-forward "\t ")
	(cond
	 ((looking-at "(\\*")
	  ;; Indentation for comments alone on a line, matches the
	  ;; proper indentation of the next line. Search only for the
	  ;; next "*)", not for the matching.
	  ;; (if (not (search-forward "*)" nil t))
	  ;;    (error "Comment not ended."))

	  ;; find matching close comment ... need reverse search too
	  (if (not (sml-find-comment-end))
	     (error "Comment not ended."))
	  (end-of-line)
	  (skip-chars-forward "*\n\t ")
	  (let ((result
		  ;; If we are at eob, just indent 0
		  (if (eobp) 0 (sml-calculate-indentation))))
	    ;; (message (format "Indentation %d" result))
	    result))
	 ;; Continued string ? (Added 890113 lbn)
	 ((looking-at "\\\\")
	  (save-excursion
	    (if (save-excursion (previous-line 1)
				(beginning-of-line)
				(looking-at "[\t ]*\\\\"))
		(progn (previous-line 1) (current-indentation))
	    (if (re-search-backward "[^\\\\]\"" nil t)
		(1+ (current-indentation))
	      0))))
	 ;; Are we looking at a case expression ?
	 ((looking-at "|.*=>")
	  (sml-skip-block)
	  (sml-re-search-backward "=>")
	  ;; Dont get fooled by fn _ => in case statements (890726)
	  ;; Changed the regexp a bit, so fn has to be first on line,
	  ;; in order to let the loop continue (Used to be ".*\bfn....")
	  ;; (900430).
	  (let ((loop t))
	    (while (and loop (save-excursion
			       (beginning-of-line)
			       (looking-at "[^ \t]+\\bfn\\b.*=>")))
	      (setq loop (sml-re-search-backward "=>"))))
	  (beginning-of-line)
	  (skip-chars-forward "\t ")
	  (cond
	   ((looking-at "|") (current-indentation))
	   ((and sml-case-indent (looking-at "of\\b"))
	    (1+ (current-indentation)))
	   ((looking-at "fn\\b") (1+ (current-indentation)))
	   ((looking-at "handle\\b") (+ (current-indentation) 5))
	   (t (+ (current-indentation) sml-pipe-indent))))
	 ((looking-at "and\\b")
	  (if (sml-find-matching-starter sml-starters-reg)
	      (current-column)
	    0))
	 ((looking-at "in\\b")		; Match the beginning let/local
	  (sml-find-match-indent "in" "\\bin\\b" "\\blocal\\b\\|\\blet\\b"))
	 ((looking-at "end\\b")		; Match the beginning
	  (sml-find-match-indent "end" "\\bend\\b" sml-end-starters-reg))
	 ((and sml-nested-if-indent (looking-at "else[\t ]*if\\b"))
	  (sml-re-search-backward "\\bif\\b\\|\\belse\\b")
	  (current-indentation))
	 ((looking-at "else\\b")	; Match the if
	  (sml-find-match-indent "else" "\\belse\\b" "\\bif\\b" t))
	 ((looking-at "then\\b")	; Match the if + extra indentation
	  (+ (sml-find-match-indent "then" "\\bthen\\b" "\\bif\\b" t)
	     sml-indent-level))
	 ((and sml-case-indent (looking-at "of\\b"))
	  (sml-re-search-backward "\\bcase\\b")
	  (+ (current-column) 2))
	 ((looking-at sml-starters-reg)
	  (let ((start (point)))
	    (sml-backward-sexp)
	    (if (and (looking-at sml-starters-indent-after)
		     (/= start (point)))
		(+ (if sml-type-of-indent
		       (current-column)
		     (if (progn (beginning-of-line)
				(skip-chars-forward "\t ")
				(looking-at "|"))
			 (- (current-indentation) sml-pipe-indent)
		       (current-indentation)))
		   sml-indent-level)
	      (beginning-of-line)
	      (skip-chars-forward "\t ")
	      (if (and (looking-at sml-starters-indent-after)
		       (/= start (point)))
		  (+ (if sml-type-of-indent
			 (current-column)
		       (current-indentation))
		     sml-indent-level)
		(goto-char start)
		(if (sml-find-matching-starter sml-starters-reg)
		    (current-column)
		  0)))))
	 (t
	  (let ((indent (sml-get-indent)))
	    (cond
	     ((looking-at "|")
	      ;; Lets see if it is the follower of a function definition
	      (if (sml-find-matching-starter
		   "\\bfun\\b\\|\\bfn\\b\\|\\band\\b\\|\\bdatatype\
\\b\\|\\bhandle\\b")
		  (cond
		   ((looking-at "fun\\b") (- (current-column) sml-pipe-indent))
		   ((looking-at "fn\\b") (1+ (current-column)))
		   ((looking-at "and\\b") (1+ (1+ (current-column))))
		   ((looking-at "datatype\\b") (1+ (1+ (current-column))))
		   ((looking-at "handle\\b") (+ (current-column) 5)))
		(+ indent sml-pipe-indent)))
	     (t
	       (let ((found (sml-fun-first-line-indent)))
		 (if (car found)
		     (cdr found)
		   (if sml-paren-lookback ; Look for open parenthesis ?
		       (max indent (sml-get-paren-indent))
		     indent))))))))))))

(defun sml-find-comment-end ()
  ;; looking at (* ... (not *))
  (let ((depth 1))
    (let ((result
	    (progn
	      (if (looking-at "(\\*)")
		  (progn (forward-char 3) (setq depth 0))
		(forward-char))
	      (if (> depth 0)
		(if (re-search-forward ".\\*)\\|(\\*.\\|(\\*)" nil t)
		    (progn
		      (backward-char 3)
		      (cond ((looking-at ".\\*)") (setq depth (- depth 1)))
			    ((looking-at "(\\*.") (setq depth (+ depth 1))))
		      (forward-char 2)
		      (setq result t))
		  (setq result nil) ))) ))
      (if result (forward-char))
      result)))

(defun sml-fun-first-line-indent ()
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (skip-chars-forward "\t ")
    (if (or (looking-at "fun\\b")
	    (looking-at "fn\\b")
	    (looking-at "and\\b")
	    (looking-at "datatype\\b")
	    (looking-at "handle\\b"))
	(cons t
	      (+ (current-indentation)
		 (cond
		   ((looking-at "fun\\b") 4)
		   ((looking-at "fn\\b") 5)
		   ((looking-at "and\\b") 4)
		   ((looking-at "datatype\\b") 4)
		   ((looking-at "handle\\b") 7) )))
      (cons nil nil) )))

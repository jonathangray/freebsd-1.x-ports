;; ml.el. Major mode for editing (Standard) ML.
;; Hacked by Olin Shivers for comint from Lars Bo Nielsen's sml.el.
;; This file is under construction.

;; These things should be done. I have no intention of doing them. -Olin
;; - comprehend and improve the indentation/parsing. Ouch.
;;   + Do mark/send-defun make sense?
;;
;; - Port Adam Dingle's debug package.
;;
;; - The process stuff could be factored into a separate file, mlproc.el,
;;   I haven't done it, lacking a good reason to do so.
;;
;; - SML/NJ's parser needs to generate correct error reports for std_in text.
;;
;; - Interface to SourceGroup system is needed.
;;
;; - ML should be given a "back door" for the emacs process interface --
;;   communication via a socket or something, instead of multiplexing
;;   the std_in/std_out stream with the user's and emacs' interactions.
;;
;; - Epoch/emacs 19 highlighting would be good for the error reporting.
;;   The debugger could also profitably make use of Epoch features.
;;
;; - Some good documentation should be written in latexinfo for the mode.
;;   The soar-mode latexinfo node would probably make a good start for this.


;; YOUR .EMACS FILE
;;=============================================================================
;; Some suggestions for your .emacs file.
;;
;; ; If ml.el lives in some non-standard directory, you must tell emacs
;; ; where to get it. This may or may not be necessary.
;; (setq load-path (cons (expand-file-name "~jones/lib/emacs") load-path))
;;
;; ; Autoload ml and ml-mode from file ml.el
;; (autoload 'ml "ml" "Run an inferior ML process." t)
;; (autoload 'ml-mode "ml" "Major mode for editing ML source." t)
;;
;; ; Files ending in ".sml" are ML source, so put their buffers in ml-mode.
;; (setq auto-mode-alist
;;       (cons '("\\.sml$" . ml-mode) 
;;	       auto-mode-alist))   
;;
;; ; The binary has an odd name or location:
;; (setq ml-program-name "/usr/local/beta-test/sml")
;; 
;; ; Define C-c t to run my favorite command in inferior ML mode:
;; (setq ml-load-hook
;;       '((lambda () (define-key inferior-ml-mode-map "\C-ct"
;;                                'favorite-cmd))))


(provide 'ml)
(require 'comint)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONSTANTS CONTROLLING THE MODE.
;;;
;;; These are the constants you might want to change
;;; 

(defvar ml-indent-level 4
  "*Indentation of blocks in ML.")

(defvar ml-pipe-indent -2
  "*Extra (negative) indentation for lines beginning with |.")

(defvar ml-case-indent nil
  "*How to indent case-of expressions.
 If t:   case expr              If nil:   case expr of
           of exp1 => ...                     exp1 => ...
            | exp2 => ...                   | exp2 => ...

The first seems to be the standard in NJ-SML, but the second
is the default.")

(defvar ml-nested-if-indent nil
  "*If set to t, nested if-then-else expression will have the same
indentation as:
                 if exp1 then exp2
                 else if exp3 then exp4
                 else if exp5 then exp6
                      else exp7")

(defvar ml-type-of-indent t
  "*How to indent `let' `struct' etc.
 If t:  fun foo bar = let              If nil:  fun foo bar = let
                          val p = 4                 val p = 4
                      in                        in
                          bar + p                   bar + p
                      end                       end

Will not have any effect if the starting keyword is first on the line.")

(defvar ml-electric-semi-mode nil
  "*If t, a `\;' will insert itself, reindent the line, and perform a newline.
If nil, just insert a `\;'. (To insert while t, do: C-q \;).")

(defvar ml-paren-lookback 1000
  "*Determines how far back (in chars) the indentation algorithm
should look for open parenthesis. High value means slow indentation
algorithm. A value of 200 (being the equivalent of 4-6 lines) should
suffice most uses. (A value of nil, means do not look at all)")

(defvar ml-program-name "sml"
  "*Program to run as ML.")

(defvar ml-use-command "use \"%s\""
  "*Template for loading a file into the inferior ML.
Set to \"use \\\"%s\\\" for SML of NJ; \"use [\\\"%s\\\"]\" for Edinburgh SML.")

(defvar ml-import-command "import \"%s\""
  "*Template for importing a file into the inferior ML.")

(defvar ml-cd-command "System.Directory.cd \"%s\""
  "ML command for changing directories in ML process.
See emacs command sml-cd.")

(defvar ml-prompt-regexp "^[\-=] *"
  "*Regexp used to recognise prompts in the inferior ML process.")

(defvar ml-temp-threshold 0
  "*Controls when emacs uses temporary files to communicate with ML. 
If not a number (e.g., NIL), then emacs always sends text directly to the
subprocess.  If an integer N, then emacs uses a temporary file whenever the
text is longer than N chars. ML-TEMP-FILE contains the name of the temporary
file for communicating. See variable ML-USE-COMMAND and function
ML-SEND-REGION.

Sending regions directly through the pty (not using temp files) doesn't
work very well. SML/NJ doesn't correctly report the line # of errors
occurring in std_in. Regions not ending in a semi-colon aren't
terminated by the parser when read from std_in. Some operating systems
have small pty buffers, and large regions will overflow. Some operating
systems (e.g., MIPS') initialise pty's with # as the tty erase character,
so that #'s in source text will disappear, along with the preceding character.")

(defvar ml-temp-file (make-temp-name "/tmp/ml")
  "*Temp file that emacs uses to communicate with the ML process.
See ML-TEMP-THRESHOLD. Defaults to (MAKE-TEMP-NAME \"/tmp/ml\")")


;;; These bindings have the following advantages over sml.el:
;;; - C-M-\ is the standard emacs binding for indent-region.
;;; - Sml.el has several bindings of the form C-c <letter>.
;;;   These bindings are not supposed to be used by modes; they
;;;   are reserved for user customisation.
;;; - Bindings are more compatible with cmuscheme.el, cmulisp.el,
;;;   tea.el, hemlock, Zwei, and other process-in-buffer emacs
;;;   interfaces.

;;; Need: mark-defun, m-c-x

;;; Install the bindings common to the source and process modes:
(defun install-ml-keybindings (map)
  ;; Process commands:
  (define-key map "\C-c\C-l" 'ml-load-file)
  (define-key map "\C-c\C-k" 'ml-import-file)	; k = kompile
  (define-key map "\C-c\C-r" 'ml-send-region)
; (define-key map "\C-c\C-e" 'ml-send-function) ; Bogus
  (define-key map "\C-c`" 'ml-next-error)
  (define-key map "\C-c=" 'ml-goto-error)
  ;; Text-formatting commands:
  (define-key map "\M-|" 'ml-electric-pipe)
  (define-key map "\;" 'ml-electric-semi)
  (define-key map "\M-\t" 'ml-back-to-outer-indent)
  (define-key map "\C-j" 'newline-and-indent)
  (define-key map "\177" 'backward-delete-char-untabify)
  (define-key map "\C-\M-\\" 'ml-indent-region)
  (define-key map "\t" 'ml-indent-line))

(defvar ml-mode-map nil "The mode map used in ml-mode.")
(cond ((not ml-mode-map)
       (setq ml-mode-map (make-sparse-keymap))
       (install-ml-keybindings ml-mode-map)
       (define-key ml-mode-map "\C-c\C-z" 'switch-to-ml)))

;;; THIS NEEDS WORK
(defvar ml-mode-syntax-table nil "The syntax table used in ml-mode.")
(if ml-mode-syntax-table
    ()
  (setq ml-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()1" ml-mode-syntax-table)
  (modify-syntax-entry ?\) ")(4" ml-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" ml-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" ml-mode-syntax-table)
  (modify-syntax-entry ?\' "'" ml-mode-syntax-table))


(defun ml-mode ()
  "Major mode for editing ML code.
Tab indents for ML code.
Comments are delimited with (* ... *).
Blank lines and form-feeds (^L's) separate paragraphs.
Delete converts tabs to spaces as it moves back.

For information on running an inferior ML process, see the documentation
for inferior-ml-mode.

Customisation: Entry to this mode runs the hooks on ml-mode-hook.

Variables controlling the indentation
=====================================

ml-indent-level (default 4)
    The indentation of a block of code.

ml-pipe-indent (default -2)
    Extra indentation of a line starting with \"|\".

ml-case-indent (default nil)
    Determine the way to indent case-of expression.
    If t:   case expr              If nil:   case expr of
              of exp1 => ...                     exp1 => ...
               | exp2 => ...                   | exp2 => ...

    The first seems to be the standard in NJ-SML. The second is the default.

ml-nested-if-indent (default nil)
    If set to t, nested if-then-else expression will have the same
    indentation as:
                     if exp1 then exp2
                     else if exp3 then exp4
                     else if exp5 then exp6
                          else exp7

ml-type-of-indent (default t)
    How to indent `let' `struct' etc.

    If t:  fun foo bar = let                If nil:  fun foo bar = let
                             val p = 4                   val p = 4
                         in                          in
                             bar + p                     bar + p
                         end                         end

    Will not have any effect if the starting keyword is first on the line.

ml-electric-semi-mode (default t)
    If t, a `\;' will reindent line, and perform a newline.

ml-paren-lookback (default 200)
    Determines how far back (in chars) the indentation algorithm
    should look for open parenthesis. High value means slow indentation
    algorithm. A value of 200 (being the equivalent of 4-6 lines) should
    suffice most uses. (A value of nil, means do not look at all)

Mode map
========
\\{ml-mode-map}"

  (interactive)
  (kill-all-local-variables)
  (ml-mode-variables)
  (use-local-map ml-mode-map)
  (setq major-mode 'ml-mode)
  (setq mode-name "ML")
  (run-hooks 'ml-mode-hook))		; Run the hook

;; What is the deal? This is a symbol, but it's also defined as a var?
(define-abbrev-table 'ml-mode-abbrev-table ())

(defun ml-mode-variables ()
  (set-syntax-table ml-mode-syntax-table)
  (setq local-abbrev-table ml-mode-abbrev-table)
  ;; A paragraph is separated by blank lines or ^L only.
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^[\t ]*$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'ml-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'comment-column)
  (setq comment-column 40)		
  (make-local-variable 'comment-start-skip)
  ;; This matches a start of comment (I sure hope!)
  (setq comment-start-skip "(\\*+[ \t]?")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'ml-comment-indent)
  ;;
  ;; Adding these will fool the matching of parens. I really don't
  ;; know why. It would be nice to have comments treated as
  ;; white-space
  ;; 
  ;; (make-local-variable 'parse-sexp-ignore-comments)
  ;; (setq parse-sexp-ignore-comments t)
  )

(defconst ml-pipe-matchers-reg
  "\\bcase\\b\\|\\bfn\\b\\|\\bfun\\b\\|\\bhandle\\b\
\\|\\bdatatype\\b\\|\\babstype\\b\\|\\band\\b"
  "The keywords a `|' can follow.")

(defun ml-electric-pipe ()
  "Insert a \"|\". Depending on the context insert the name of
function, a \"=>\" etc."
  (interactive)
  (let ((case-fold-search nil)		; Case sensitive
	(here (point))
	(match (save-excursion
		 (ml-find-matching-starter ml-pipe-matchers-reg)
		 (point)))
	(tmp "  => ")
	(case-or-handle-exp t))
    (if (/= (save-excursion (beginning-of-line) (point))
	    (save-excursion (skip-chars-backward "\t ") (point)))
	(insert "\n"))
    (insert "|")
    (save-excursion
      (goto-char match)
      (cond
       ;; It was a function, insert the function name
       ((looking-at "fun\\b")
	(setq tmp (concat " " (buffer-substring
			       (progn (forward-char 3)
				      (skip-chars-forward "\t\n ") (point))
			       (progn (forward-word 1) (point))) " "))
	(setq case-or-handle-exp nil))
       ;; It was a datatype, insert nothing
       ((looking-at "datatype\\b\\|abstype\\b")
	(setq tmp " ") (setq case-or-handle-exp nil))
       ;; If it is an and, then we have to see what is was
       ((looking-at "and\\b")
	(let (isfun)
	  (save-excursion
	    (condition-case ()
		(progn
		  (re-search-backward "datatype\\b\\|abstype\\b\\|fun\\b")
		  (setq isfun (looking-at "fun\\b")))
	      (error (setq isfun nil))))
	  (if isfun
	      (progn
		(setq tmp
		      (concat " " (buffer-substring
				   (progn (forward-char 3)
					  (skip-chars-forward "\t\n ") (point))
				   (progn (forward-word 1) (point))) " "))
		(setq case-or-handle-exp nil))
	    (setq tmp " ") (setq case-or-handle-exp nil))))))
    (insert tmp)
    (ml-indent-line)
    (beginning-of-line)
    (skip-chars-forward "\t ")
    (forward-char (1+ (length tmp)))
    (if case-or-handle-exp
	(forward-char -4))))

(defun ml-electric-semi ()
  "Inserts a \;.
If ml-electric-semi-mode is t, indent the current line, and newline."
  (interactive)
  (insert "\;")
  (if ml-electric-semi-mode
      (reindent-then-newline-and-indent)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSING ERROR MESSAGES
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The error message is currently supported for SML/NJ's error messages,
;;; but the code is structured to allow other systems to be handled.
;;; See ML-ERROR-PARSER.

;;; The reg-expression used when looking for errors. SML/NJ errors look like 
;;; this:
;;; std_in:2.1-4.3 Error: operator and operand don't agree (tycon mismatch)
;;; 
;;; Sometimes, the error report gives just a point, not a range:
;;; std_in:2.1 Error: operator and operand don't agree (tycon mismatch)
;;;
;;; If you don't want warnings to show up, change the regexp to:
;;; "^.+:[0-9]+\\.[0-9]+.+Error:"

(defvar ml-error-regexp
  "^.+:[0-9]+\\.[0-9]+.+\\(Error\\|Warning\\):"
  "Regexp for matching error.")

;; This function parses the error message into a 3 or 5 element list:
;;     (file start-line start-col [end-line end-col])
(setq ml-error-parser
  (function (lambda (pt)
    (save-excursion
      (goto-char pt)
      (re-search-forward "^[-= ]*\\(.+\\):\
\\([0-9]+\\)\\.\\([0-9]+\\)\\(-\\([0-9]+\\)\\.\\([0-9]+\\)\\)?\
.+\\(Error\\|Warning\\):")
      (let ((tail (and (match-beginning 4)
		       (list (string-to-int (buffer-substring        ; end line
					     (match-beginning 5)
					     (match-end 5)))
			     (1- (string-to-int (buffer-substring    ; end col
						 (match-beginning 6)
						 (match-end 6))))))))
	(nconc (list (buffer-substring (match-beginning 1)       ; file
					 (match-end 1))
		     (string-to-int (buffer-substring		 ; start line
				       (match-beginning 2)
				       (match-end 2)))
		     (1- (string-to-int (buffer-substring        ; start col
					 (match-beginning 3)
					 (match-end 3)))))
	       tail))))))

(defun ml-next-error ()
  "Find the next error by parsing the inferior ML buffer.
Move the error message on the top line of the window;
put the cursor at the beginning of the error source. If the
error message specifies a range, the mark is placed at the end. 

Error interaction has several limitations:
- It is only implemented for SML/NJ, although it should be fairly easy to
  port to other implementations.
- It won't work for text entered at the prompt (read from std_in)
  because the SML/NJ parser doesn't report line numbers correctly.
- This means that source sent via send-region must communicate
  with the inferior ML process via temp files, not by stuffing
  the source down the pty. (See variable ml-temp-threshold.)
- And even then, ml-mode only handles the most recent chunk of text
  sent via send-region."
  (interactive)
  (let ((case-fold-search nil))
    (pop-to-buffer (ml-proc-buffer))
    (goto-char ml-error-cursor)	; Goto last found error
    (if (re-search-forward ml-error-regexp (point-max) t) ; go to next err
	(let* ((parse (funcall ml-error-parser (match-beginning 0)))
	       (file (nth 0 parse))
	       (line0 (nth 1 parse))
	       (col0 (nth 2 parse))
	       (line/col1 (cdr (cdr (cdr parse)))))
	  (set-marker ml-error-cursor (point))

	  (set-window-start (get-buffer-window (ml-proc-buffer))
			    (save-excursion (beginning-of-line) (point)))
	  (cond ((equal file "std_in")
		 (error "Can't track error reports from std_in."))

		;; Errorful input came from temp file. 
		((equal file ml-temp-file)
		 (if (< (point) ml-error-barrier)
		     (error "Temp file error report not current.")
		     (let ((file (car ml-real-file))
			   (pos (cdr ml-real-file)))
		       (cond ((not file)
			      (error "No source file."))
			     ((not (file-readable-p file))
			      (error (concat "Can't read file " file)))
			     (t
			      (find-file-other-window file)
			      ;; If given, put mark at line/col1:
			      (cond (line/col1
				     (goto-char pos)
				     (if (> (car line/col1) 1)
					 (forward-line (1- (car line/col1))))
				     (forward-char (1+ (car (cdr line/col1))))
				     (push-mark)))
			      (goto-char pos)
			      (if (> line0 1) (forward-line (1- line0)))
			      (forward-char col0))))))
		     
		;; Errorful input came from a source file.
		((file-readable-p file)
		 (switch-to-buffer-other-window (find-file-noselect file))
		 ;; If given, put mark at line/col1:
		 (cond (line/col1
			(goto-line (car line/col1))
			(forward-char (1+ (car (cdr line/col1))))
			(push-mark)))
		 (goto-line line0) (forward-char col0))
		(t
		 (error (concat "Can't read file " file)))))

	(error "No error message found."))))


(defun ml-goto-error ()
  "Go to the error reported on the current line 
and set the error cursor to this line."
  (interactive)
  (beginning-of-line)
  (set-marker ml-error-cursor (point))
  (ml-next-error))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INDENTATION
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ml-indent-region (begin end)
  "Indent region of ML code."
  (interactive "r")
  (message "Indenting region...")
  (save-excursion
    (goto-char end) (setq end (point-marker)) (goto-char begin)
    (while (< (point) end)
      (skip-chars-forward "\t\n ")
      (ml-indent-line)
      (end-of-line))
    (move-marker end nil))
  (message "Indenting region... done"))

(defun ml-indent-line ()
  "Indent current line of ML code."
  (interactive)
  (let ((indent (ml-calculate-indentation)))
    (if (/= (current-indentation) indent)
	(save-excursion			;; Added 890601 (point now stays)
	  (let ((beg (progn (beginning-of-line) (point))))
	    (skip-chars-forward "\t ")
	    (delete-region beg (point))
	    (indent-to indent))))
    ;; If point is before indentation, move point to indentation
    (if (< (current-column) (current-indentation))
	(skip-chars-forward "\t "))))

(defun ml-back-to-outer-indent ()
  "Unindents to the next outer level of indentation."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "\t ")
    (let ((start-column (current-column))
	  (indent (current-column)))
      (if (> start-column 0)
	  (progn
	    (save-excursion
	      (while (>= indent start-column)
		(if (re-search-backward "^[^\n]" nil t)
		    (setq indent (current-indentation))
		  (setq indent 0))))
	    (backward-delete-char-untabify (- start-column indent)))))))

(defconst ml-indent-starters-reg
  "abstraction\\b\\|abstype\\b\\|and\\b\\|case\\b\\|datatype\\b\
\\|else\\b\\|fun\\b\\|functor\\b\\|if\\b\\|sharing\\b\
\\|in\\b\\|infix\\b\\|infixr\\b\\|let\\b\\|local\\b\
\\|nonfix\\b\\|of\\b\\|open\\b\\|raise\\b\\|sig\\b\\|signature\\b\
\\|struct\\b\\|structure\\b\\|then\\b\\|\\btype\\b\\|val\\b\
\\|while\\b\\|with\\b\\|withtype\\b"
  "The indentation starters. The next line, after one starting with
one of these, will be indented.")

(defconst ml-starters-reg
  "\\babstraction\\b\\|\\babstype\\b\\|\\bdatatype\\b\
\\|\\bexception\\b\\|\\bfun\\b\\|\\bfunctor\\b\\|\\blocal\\b\
\\|\\binfix\\b\\|\\binfixr\\b\\|\\bsharing\\b\
\\|\\bnonfix\\b\\|\\bopen\\b\\|\\bsignature\\b\\|\\bstructure\\b\
\\|\\btype\\b\\|\\bval\\b\\|\\bwithtype\\b\\|\\bwith\\b"
  "The starters of new expressions.")

(defconst ml-end-starters-reg
  "\\blet\\b\\|\\blocal\\b\\|\\bsig\\b\\|\\bstruct\\b\\|\\bwith\\b"
  "Matching reg-expression for the \"end\" keyword.")

(defconst ml-starters-indent-after
  "let\\b\\|local\\b\\|struct\\b\\|in\\b\\|sig\\b\\|with\\b"
  "Indent after these.")

(defun ml-calculate-indentation ()
  (save-excursion
    (let ((case-fold-search nil))
      (beginning-of-line)
      (if (bobp)			; Beginning of buffer
	  0				; Indentation = 0
	(skip-chars-forward "\t ")
	(cond
	 ;; Indentation for comments alone on a line, matches the
	 ;; proper indentation of the next line. Search only for the
	 ;; next "*)", not for the matching.
	 ((looking-at "(\\*")
	  (if (not (search-forward "*)" nil t))
	      (error "Comment not ended."))
	  (end-of-line)
	  (skip-chars-forward "\n\t ")
	  ;; If we are at eob, just indent 0
	  (if (eobp) 0 (ml-calculate-indentation)))
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
	  (ml-skip-block)
	  (ml-re-search-backward "=>")
	  ;; Dont get fooled by fn _ => in case statements (890726)
	  ;; Changed the regexp a bit, so fn has to be first on line,
	  ;; in order to let the loop continue (Used to be ".*\bfn....")
	  ;; (900430).
	  (let ((loop t))
	    (while (and loop (save-excursion
			       (beginning-of-line)
			       (looking-at "[^ \t]+\\bfn\\b.*=>")))
	      (setq loop (ml-re-search-backward "=>"))))
	  (beginning-of-line)
	  (skip-chars-forward "\t ")
	  (cond
	   ((looking-at "|") (current-indentation))
	   ((and ml-case-indent (looking-at "of\\b"))
	    (1+ (current-indentation)))
	   ((looking-at "fn\\b") (1+ (current-indentation)))
	   ((looking-at "handle\\b") (+ (current-indentation) 5))
	   (t (+ (current-indentation) ml-pipe-indent))))
	 ((looking-at "and\\b")
	  (if (ml-find-matching-starter ml-starters-reg)
	      (current-column)
	    0))
	 ((looking-at "in\\b")		; Match the beginning let/local
	  (ml-find-match-indent "in" "\\bin\\b" "\\blocal\\b\\|\\blet\\b"))
	 ((looking-at "end\\b")		; Match the beginning
	  (ml-find-match-indent "end" "\\bend\\b" ml-end-starters-reg))
	 ((and ml-nested-if-indent (looking-at "else[\t ]*if\\b"))
	  (ml-re-search-backward "\\bif\\b\\|\\belse\\b")
	  (current-indentation))
	 ((looking-at "else\\b")	; Match the if
	  (ml-find-match-indent "else" "\\belse\\b" "\\bif\\b" t))
	 ((looking-at "then\\b")	; Match the if + extra indentation
	  (+ (ml-find-match-indent "then" "\\bthen\\b" "\\bif\\b" t)
	     ml-indent-level))
	 ((and ml-case-indent (looking-at "of\\b"))
	  (ml-re-search-backward "\\bcase\\b")
	  (+ (current-column) 2))
	 ((looking-at ml-starters-reg)
	  (let ((start (point)))
	    (ml-backward-sexp)
	    (if (and (looking-at ml-starters-indent-after)
		     (/= start (point)))
		(+ (if ml-type-of-indent
		       (current-column)
		     (if (progn (beginning-of-line)
				(skip-chars-forward "\t ")
				(looking-at "|"))
			 (- (current-indentation) ml-pipe-indent)
		       (current-indentation)))
		   ml-indent-level)
	      (beginning-of-line)
	      (skip-chars-forward "\t ")
	      (if (and (looking-at ml-starters-indent-after)
		       (/= start (point)))
		  (+ (if ml-type-of-indent
			 (current-column)
		       (current-indentation))
		     ml-indent-level)
		(goto-char start)
		(if (ml-find-matching-starter ml-starters-reg)
		    (current-column)
		  0)))))
	 (t
	  (let ((indent (ml-get-indent)))
	    (cond
	     ((looking-at "|")
	      ;; Lets see if it is the follower of a function definition
	      (if (ml-find-matching-starter
		   "\\bfun\\b\\|\\bfn\\b\\|\\band\\b\\|\\bhandle\\b")
		  (cond
		   ((looking-at "fun\\b") (- (current-column) ml-pipe-indent))
		   ((looking-at "fn\\b") (1+ (current-column)))
		   ((looking-at "and\\b") (1+ (1+ (current-column))))
		   ((looking-at "handle\\b") (+ (current-column) 5)))
		(+ indent ml-pipe-indent)))
	     (t
	      (if ml-paren-lookback	; Look for open parenthesis ?
		  (max indent (ml-get-paren-indent))
		indent))))))))))

(defun ml-get-indent ()
  (save-excursion
    (let ((case-fold-search nil))
      (beginning-of-line)
      (skip-chars-backward "\t\n; ")
      (if (looking-at ";") (ml-backward-sexp))
      (cond
       ((save-excursion (ml-backward-sexp) (looking-at "end\\b"))
	(- (current-indentation) ml-indent-level))
       (t
	(while (/= (current-column) (current-indentation))
	  (ml-backward-sexp))
	(skip-chars-forward "\t |")
	(let ((indent (current-column)))
	  (skip-chars-forward "\t (")
	  (cond
	   ;; Started val/fun/structure...
	   ((looking-at ml-indent-starters-reg)
	    (+ (current-column) ml-indent-level))
	   ;; Indent after "=>" pattern, but only if its not an fn _ =>
	   ;; (890726)
	   ((looking-at ".*=>")
	    (if (looking-at ".*\\bfn\\b.*=>")
		indent
	      (+ indent ml-indent-level)))
	   ;; else keep the same indentation as previous line
	   (t indent))))))))

(defun ml-get-paren-indent ()
  (save-excursion
    (let ((levelpar 0)			; Level of "()"
          (levelcurl 0)                 ; Level of "{}"
          (levelsqr 0)                  ; Level of "[]"
          (backpoint (max (- (point) ml-paren-lookback) (point-min))))
      (catch 'loop
	(while (and (/= levelpar 1) (/= levelsqr 1) (/= levelcurl 1))
	  (if (re-search-backward "[][{}()]" backpoint t)
	      (if (not (ml-inside-comment-or-string-p))
		  (cond
		   ((looking-at "(") (setq levelpar (1+ levelpar)))
		   ((looking-at ")") (setq levelpar (1- levelpar)))
		   ((looking-at "\\[") (setq levelsqr (1+ levelsqr)))
		   ((looking-at "\\]") (setq levelsqr (1- levelsqr)))
		   ((looking-at "{") (setq levelcurl (1+ levelcurl)))
		   ((looking-at "}") (setq levelcurl (1- levelcurl)))))
	    (throw 'loop 0)))		; Exit with value 0
	(if (save-excursion
	      (forward-char 1)
	      (looking-at ml-indent-starters-reg))
	    (1+ (+ (current-column) ml-indent-level))
	  (1+ (current-column)))))))

;; This is too slow
;;
;; (defun ml-inside-comment-or-string-p ()
;;  (let ((state (parse-partial-sexp (point-min) (point))))
;;    (or (nth 4 state) (nth 3 state))))

(defun ml-inside-comment-or-string-p ()
  (let ((start (point)))
    (if (save-excursion
	  (condition-case ()
	      (progn
		(search-backward "(*")
		(search-forward "*)")
		(forward-char -1)	; A "*)" is not inside the comment
		(> (point) start))
	    (error nil)))
	t
      (let ((numb 0))
	(save-excursion
	  (save-restriction
	    (narrow-to-region (progn (beginning-of-line) (point)) start)
	    (condition-case ()
		(while t
		  (search-forward "\"")
		  (setq numb (1+ numb)))
	      (error (if (and (not (zerop numb))
			      (not (zerop (% numb 2))))
			 t nil)))))))))

(defun ml-skip-block ()
  (let ((case-fold-search nil))
    (ml-backward-sexp)
    (if (looking-at "end\\b")
	(progn
	  (goto-char (ml-find-match-backward "end" "\\bend\\b"
					      ml-end-starters-reg))
	  (skip-chars-backward "\n\t "))
      ;; Here we will need to skip backward past if-then-else
      ;; and case-of expression. Please - tell me how !!
      )))

(defun ml-find-match-backward (unquoted-this this match &optional start)
  (save-excursion
    (let ((case-fold-search nil)
	  (level 1)
	  (pattern (concat this "\\|" match)))
      (if start (goto-char start))
      (while (not (zerop level))
	(if (ml-re-search-backward pattern)
	    (setq level (cond
			 ((looking-at this) (1+ level))
			 ((looking-at match) (1- level))))
	  ;; The right match couldn't be found
	  (error (concat "Unbalanced: " unquoted-this))))
      (point))))

(defun ml-find-match-indent (unquoted-this this match &optional indented)
  (save-excursion
    (goto-char (ml-find-match-backward unquoted-this this match))
    (if (or ml-type-of-indent indented)
	(current-column)
      (if (progn
	    (beginning-of-line)
	    (skip-chars-forward "\t ")
	    (looking-at "|"))
	  (- (current-indentation) ml-pipe-indent)
	(current-indentation)))))

(defun ml-find-matching-starter (regexp)
  (let ((case-fold-search nil)
	(start-let-point (ml-point-inside-let-etc))
	(start-up-list (ml-up-list))
	(found t))
    (if (ml-re-search-backward regexp)
	(progn
	  (condition-case ()
	      (while (or (/= start-up-list (ml-up-list))
			 (/= start-let-point (ml-point-inside-let-etc)))
		(re-search-backward regexp))
	    (error (setq found nil)))
	  found)
      nil)))

(defun ml-point-inside-let-etc ()
  (let ((case-fold-search nil) (last nil) (loop t) (found t) (start (point)))
    (save-excursion
      (while loop
	(condition-case ()
	    (progn
	      (re-search-forward "\\bend\\b")
	      (while (ml-inside-comment-or-string-p)
		(re-search-forward "\\bend\\b"))
	      (forward-char -3)
	      (setq last (ml-find-match-backward "end" "\\bend\\b"
						  ml-end-starters-reg last))
	      (if (< last start)
		  (setq loop nil)
		(forward-char 3)))
	  (error (progn (setq found nil) (setq loop nil)))))
      (if found
	  last
	0))))

(defun ml-re-search-backward (regexpr)
  (let ((case-fold-search nil) (found t))
    (if (re-search-backward regexpr nil t)
	(progn
	  (condition-case ()
	      (while (ml-inside-comment-or-string-p)
		(re-search-backward regexpr))
	    (error (setq found nil)))
	  found)
      nil)))

(defun ml-up-list ()
  (save-excursion
    (condition-case ()
	(progn
	  (up-list 1)
	  (point))
      (error 0))))

(defun ml-backward-sexp ()
  (condition-case ()
      (progn
	(let ((start (point)))
	  (backward-sexp 1)
	  (while (and (/= start (point)) (looking-at "(\\*"))
	    (setq start (point))
	    (backward-sexp 1))))
    (error (forward-char -1))))

(defun ml-comment-indent ()
  (if (looking-at "^(\\*")		; Existing comment at beginning
      0					; of line stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column))	; Else indent at comment column
	   comment-column))))		; except leave at least one space.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ML Process Code
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar inferior-ml-mode-map nil)
(cond ((not inferior-ml-mode-map)
       (setq inferior-ml-mode-map
	     (full-copy-sparse-keymap comint-mode-map))
       (install-ml-keybindings inferior-ml-mode-map)))

(defun ml (&optional cmd)
  "Run an inferior ML process, input and output via buffer *ml*.
If there is a process already running in *ml*, just switch to that buffer.
With argument, allows you to edit the command line (default is value
of ml-program-name).  Runs the hooks from inferior-ml-mode-hook (after the
comint-mode-hook is run).

\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive (list (and current-prefix-arg
			  (read-string "Run ML: " ml-program-name))))
  (let ((cmd (or cmd ml-program-name)))
    (if (not (comint-check-proc "*ml*"))
	(let ((cmdlist (ml-args-to-list cmd)))
	  (set-buffer (apply 'make-comint "ml" (car cmdlist)
			     nil (cdr cmdlist)))
	  (inferior-ml-mode))))
  (setq ml-buffer "*ml*")
  (switch-to-buffer "*ml*"))

(defun ml-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
	  ((not (= where 0))
	   (cons (substring string 0 where)
		 (ml-args-to-list (substring string (+ 1 where)
					     (length string)))))
	  (t (let ((pos (string-match "[^ \t]" string)))
	       (if (null pos)
		   nil
		   (ml-args-to-list (substring string pos
					       (length string)))))))))

(defun inferior-ml-mode ()
  "Major mode for interacting with an inferior ML process.

The following commands are available:
\\{inferior-ml-mode-map}

An ML process can be fired up with \\[ml].

Customisation: Entry to this mode runs the hooks on comint-mode-hook and
inferior-t-mode-hook (in that order).

You can send text to the inferior ML process from other buffers containing
ML source.  
    switch-to-ml switches the current buffer to the ML process buffer.
    ml-send-function sends the current paragraph to the ML process.
    ml-send-region sends the current region to the ML process.

    Prefixing the ml-send-function/region commands with a \\[universal-argument]
    causes a switch to the ML process buffer after sending the text.

For information on running multiple processes in multiple buffers, see
documentation for variable ml-buffer.

Commands:
Return after the end of the process' output sends the text from the 
    end of process to point.
Return before the end of the process' output copies the current line
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
Tab indents for ML; with argument, shifts rest
    of expression rigidly with the current line.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Semicolons start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (kill-all-local-variables)
  (comint-mode)
  (setq comint-prompt-regexp ml-prompt-regexp)
  (ml-mode-variables)

  ;; For sequencing through error messages:
  (make-local-variable 'ml-error-cursor)
  (setq ml-error-cursor (point-max-marker))
  (make-local-variable 'ml-error-barrier)
  (setq ml-error-barrier (point-max-marker))
  (make-local-variable 'ml-real-file)
  (setq ml-real-file (cons nil 0))

  (setq major-mode 'inferior-ml-mode)
  (setq mode-name "Inferior ML")
  (setq mode-line-process '(": %s"))
  (use-local-map inferior-ml-mode-map)
  (setq comint-input-sentinel 'ignore)
  (run-hooks 'inferior-ml-mode-hook))


;;; Fakes it with a "use <temp-file>;" if necessary.

(defun ml-send-region (start end &optional and-go)
  "Send the current region to the inferior ML process.
Prefix argument means switch-to-ml afterwards.
If the region is short, it is sent directly, via COMINT-SEND-REGION.
Otherwise, it is written to a temp file and a \"use <temp-file>;\"
command is sent to the process. See ML-TEMP-THRESHOLD, ML-TEMP-FILE
and ML-USE-COMMAND."

  (interactive "r\nP")
  (cond ((and (numberp ml-temp-threshold)
	      (< ml-temp-threshold (- end start)))
	 ;; Just in case someone is still reading from
	 ;; ml-temp-file:
	 (if (file-exists-p ml-temp-file)
	     (delete-file ml-temp-file))
	 (write-region start end ml-temp-file nil 'silently)
	 (comint-send-string (ml-proc)
		 (concat (format ml-use-command ml-temp-file) ";\n"))
	 (ml-update-barrier (buffer-file-name (current-buffer)) start)
	 (ml-update-cursor (ml-proc-buffer)))
	(t
	 (comint-send-region (ml-proc) start end)
	 (comint-send-string (ml-proc) "\n")))
  (if and-go (switch-to-ml t)))

;;; Update the buffer-local variables ML-REAL-FILE and ML-ERROR-BARRIER
;;; in the process buffer:
(defun ml-update-barrier (file pos)
  (let ((buf (current-buffer)))
    (unwind-protect (let* ((proc (ml-proc))
			   (pmark (process-mark proc)))
		      (set-buffer (process-buffer proc))
		      (setq ml-real-file
			    (and file (cons file pos)))
		      (set-marker ml-error-barrier pmark))
      (set-buffer buf))))

;;; Update the buffer-local error-cursor in PROC-BUFFER to be its
;;; current proc mark.
(defun ml-update-cursor (proc-buffer)
  (let ((buf (current-buffer)))
    (unwind-protect (let* ((proc (get-buffer-process proc-buffer)))
		      (set-buffer proc-buffer)
		      (if proc
			  (set-marker ml-error-cursor (process-mark proc))))
      (set-buffer buf))))

(defun ml-mark-function ()
  "Put mark at end of function, point at beginning.
In fact, just uses mark-paragraph."
  (interactive)
  (mark-paragraph))

;;; This is quite bogus, so we don't bind it to keys by default.

(defun ml-send-function (&optional and-go)
  "Send the current paragraph to the inferior ML process.
Prefix argument means switch-to-ml afterwards."
  (interactive "P")
  (save-excursion
    (ml-mark-function)
    (ml-send-region (point) (mark)))
  (if and-go (switch-to-ml t)))

(defun switch-to-ml (eob-p)
  "Switch to the ML process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (if (ml-proc-buffer)
      (pop-to-buffer (ml-proc-buffer))
      (error "No current process buffer. See variable ml-buffer."))
  (cond (eob-p
	 (push-mark)
	 (goto-char (point-max)))))


;;; Since ml-send-function/region take an optional prefix arg,
;;; these commands are redundant. But they are kept around for the user
;;; to bind if he wishes,  because it's easier to type C-c e than C-u C-c C-e.

(defun ml-send-region-and-go (start end)
  "Send the current region to the inferior ML process,
and switch to the process buffer."
  (interactive "r")
  (ml-send-region start end t))

(defun ml-send-function-and-go ()
  "Send the current definition to the inferior ML process, 
and switch to the process buffer."
  (interactive)
  (ml-send-function t))

(defun ml-cd (dir)
  "Change the working directory of the inferior ML process.
The directory of the process buffer is changed as well."
  (interactive "DSML Directory: ")
  (let* ((buf (ml-proc-buffer))
	 (proc (get-buffer-process buf))
	 (dir (expand-file-name dir)))
    (set-buffer buf)
    (process-send-string proc (concat (format ml-cd-command dir) ";\n"))
    (cd dir)))


;;; Loading and importing source files:
;;; ===========================================================================

(defvar ml-source-modes '(ml-mode)
  "*Used to determine if a buffer contains ML source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered an ML source file by ml-load-file and ml-compile-file.
Used by these commands to determine defaults.")

(defvar ml-prev-l/c-dir/file nil
  "Caches the (directory . file) pair used in the last ml-load-file or
ml-compile-file command. Used for determining the default in the next one.")

(defun ml-load-file (file-name)
  "Load an ML file into the inferior ML process."
  (interactive (comint-get-source "Load ML file: " ml-prev-l/c-dir/file
				  ml-source-modes t))
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq ml-prev-l/c-dir/file (cons (file-name-directory    file-name)
				   (file-name-nondirectory file-name)))
  (ml-update-cursor (ml-proc-buffer))
  (comint-send-string (ml-proc)
		      (concat (format ml-use-command file-name) ";\n"))
  (switch-to-ml t))

(defun ml-import-file (file-name)
  "Import an ML file into the inferior ML process."
  (interactive (comint-get-source "Import ML file: " ml-prev-l/c-dir/file
				  ml-source-modes nil))
  				  ; NIL because the .sml can't be there.
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq ml-prev-l/c-dir/file (cons (file-name-directory    file-name)
				   (file-name-nondirectory file-name)))
  (ml-update-cursor (ml-proc-buffer))
  (comint-send-string (ml-proc)
		      (concat (format ml-import-command file-name) ";\n"))
  (switch-to-ml t))


(defvar ml-buffer nil "*The current ML process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
Ml.el supports, in a fairly simple fashion, running multiple ML
processes. To run multiple ML processes, you start the first up with
\\[ml]. It will be in a buffer named *ml*. Rename this buffer
with \\[rename-buffer]. You may now start up a new process with another
\\[ml]. It will be in a new buffer, named *ml*. You can
switch between the different process buffers with \\[switch-to-buffer].

Commands that send text from source buffers to ML processes --
like ml-send-function or ml-compile-region -- have to choose a
process to send to, when you have more than one ML process around. This
is determined by the global variable ml-buffer. Suppose you
have three inferior ML's running:
    Buffer	Process
    foo		ml
    bar		ml<2>
    *ml*        ml<3>
If you do a \\[ml-send-function] command on some ML source code,
what process do you send it to?

- If you're in a process buffer (foo, bar, or *ml*), 
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer ml-buffer.
This process selection is performed by function ml-proc.

Whenever \\[ml] fires up a new process, it resets ml-buffer
to be the new process's buffer. If you only run one process, this will
do the right thing. If you run multiple processes, you can change
ml-buffer to another process buffer with \\[set-variable].

More sophisticated approaches are, of course, possible. If you find youself
needing to switch back and forth between multiple processes frequently, you
may wish to consider writing something like ilisp.el, a larger, more
sophisticated package for running inferior Lisp and Scheme processes. The
approach taken here is for a minimal, simple implementation. Feel free to
extend it.")

(defun ml-proc-buffer ()
  "Returns the current ML process buffer. See variable ml-buffer."
  (if (eq major-mode 'inferior-ml-mode) (current-buffer) ml-buffer))

(defun ml-proc ()
  "Returns the current ML process. See variable ml-buffer."
  (let ((proc (get-buffer-process (ml-proc-buffer))))
    (or proc
	(error "No current process. See variable ml-buffer"))))

;;; Do the user's customisation...

(defvar ml-load-hook nil
  "This hook is run when ML is loaded in.
This is a good place to put keybindings.")
	
(run-hooks 'ml-load-hook)

;;; CHANGE LOG
;;; ===========================================================================
;;; 11/91 Olin. 
;;; Created.
;;; Changes from Nielsen's sml mode:
;;; - "_" is now a symbol constituent, not a word constituent.
;;;   So M-f  and M-b usefully move around inside a variable like
;;;   a_multi_word_var, and C-M-f, C-M-b move over the entire variable.
;;; - The keybindings have changed. More compatibility with other comint-based
;;;   process modes; more politically correct (no C-c <letter> bindings).
;;; - The process stuff is brand new. M-x ml takes a useful prefix arg.
;;;   All the nice comint bits are available (M-p, M-n, M-s, etc.)
;;; - The error reporting package has been completely redone.
;;; - The source-code insertion stuff has been factored out into
;;;   a separate file, ml-forms.el. This file is just far too big.
;;;   Maybe the process stuff should be factored out, too. I suggest a
;;;   source-code insertion facility be structured along the lines
;;;   of the latex-insert-begin command in cmutex.el.
;;; - Turned off ml-electric-semi-mode by default. It was too unpredictable;
;;;   users complained.
;;;
;;; 11/91 Olin
;;; - Added a (kill-all-local-variables) to inferior-ml-mode,
;;;   because comint-mode doesn't do it anymore.
;;; - Upped ml-paren-lookback from 200 to 1000.
;;;
;;; 12/91 Olin
;;; - Added function ml-cd and variable ml-cd-command.
;;;   Features 'R Us.

; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         sun2hp.el
; Description:  noddy sun to hp assembly syntax converter
; Author:       Andy Norman, Kraken (ange@hplb.hpl.hp.com)
; Created:      Mon Sep 25 16:42:20 1989
; Modified:     Wed May  2 11:23:32 1990 (Ange) ange@anorman
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file contains an extremely noddy GNU Emacs Lisp program which will
;; attempt to translate a file containing sun format assembly code into a file
;; containing the equivalent hp format assembly code.
;; Although easy to fool, it worked OK on the 0.33 njml file M68.prim.s.


;; label object abstraction
;;
(defvar label-count 0)

(defun make-new-label ()
  (format "L%d" (setq label-count (+ 1 label-count))))

(defun make-label-object (old-label)
  (list 'label-object (make-new-label) old-label (dot-marker)))

(defun label-new-label (obj)
  (nth 1 obj))

(defun label-old-label (obj)
  (nth 2 obj))

(defun label-point (obj)
  (nth 3 obj))


;; find-all-label-definitions -- make a scan of the buffer to find all label
;; definitions, make label-object out of each one and return the lot as a list.
;;
(defun find-all-label-definitions ()
  (let (label-list)
    (goto-char (point-min))
    (while (re-search-forward "\\<\\([0-9]+\\):" (point-max) t)
      (let ((name (buffer-substring (match-beginning 1)
				    (match-end 1))))
	(setq label-list (cons (make-label-object name) label-list))))
    label-list))


;; nearest-new-label -- return the new label replacement for the old label
;; given at the point given.
;;
(defun nearest-new-label (name dir point labels)
  (let ((nearest-distance nil)
	(nearest nil))
    (while labels
      (let* ((cur (car labels))
	     (old-name (label-old-label cur)))
	(if (string-equal old-name name)
	    (let* ((cur-point (label-point cur))
		   (distance (* dir (- cur-point point))))
	      (if (>= distance 0)
		  (if (or (null nearest-distance)
			  (< distance nearest-distance))
		      (progn
			(setq nearest-distance distance)
			(setq nearest cur)))))))
      (setq labels (cdr labels)))
    (label-new-label nearest)))


;; replace-all-references-to-labels -- change all the references to the original
;; labels to references to the new set of labels.
;;
(defun replace-all-references-to-labels (labels)
  (goto-char (point-min))
  (while (re-search-forward "\\<\\([0-9]+\\)\\(b\\|f\\)" (point-max) t)
    (let* ((name (buffer-substring (match-beginning 1)
				  (match-end 1)))
	   (b-or-f (buffer-substring (match-beginning 2)
				     (match-end 2)))
	   (dir (if (string-equal b-or-f "f") 1 -1))
	   (replacement (nearest-new-label name dir (point) labels)))
      (replace-match replacement))))


;; replace-all-label-definitions -- change each of the old label 
;; definitions to their new value.
;;
(defun replace-all-label-definitions (labels)
  (while labels
    (let* ((cur (car labels))
	   (old-label (label-old-label cur))
	   (point (label-point cur))
	   (new-label (label-new-label cur)))
      (goto-char (- point 2))
      (re-search-forward "\\([0-9]+\\):" (point-max) t)
      (if (not (string-equal (buffer-substring (match-beginning 1)
					       (match-end 1))
			     old-label))
	  (error "label <%s> has moved" old-label)
	(replace-match (concat new-label ":"))))
      (setq labels (cdr labels))))


;; replace-re -- replace one regular expression with an interpreted string
;; throughout the whole buffer.
;;
(defun replace-re (regexp to-string)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp (point-max) t)
      (replace-match to-string t nil))))


;; do-subst -- substitute mnemonics, register names, comment symbols etc.
;;
(defun do-subst ()
  (replace-re "#\\((*[-~]*[0-9]\\)" "&\\1")
  (replace-re "\\(a[0-7]\\)@(\\([^)]+\\))" "\\2(\\1)")
  (replace-re "\\(sp\\)@(\\([^)]+\\))" "\\2(\\1)")
  (replace-re "sp@\\+" "(sp)\\+")
  (replace-re "sp@-" "-(sp)")
  (replace-re "sp@" "(sp)")
  (replace-re "\\<sp\\>" "%sp")
  (replace-re "\\(a[0-7]\\)@" "(\\1)")
  (replace-re "\\(a[0-7]\\)" "%\\1")
  (replace-re "\\(d[0-7]\\)" "%\\1")
  (replace-re "\\(fp[0-7]\\)" "%\\1")
  (replace-re "\\(fpcr\\)" "%\\1")
  (replace-re "\\bcc" "%cc")
  (replace-re "\\.globl" "global")
  (replace-re "\\.text" "text")
  (replace-re "\\.data" "data")
  (replace-re "\\.long" "long")
  (replace-re "\\.word" "short")
  (replace-re "\\.even" "even")
  (replace-re "\\.align" "lalign")
  (replace-re "\\.ascii" "byte")
  (replace-re "|" " #")
  (replace-re "\\<movl" "mov.l")
  (replace-re "\\<addqb" "addq.b")
  (replace-re "\\<subqb" "subq.b")
  (replace-re "\\<subw" "sub.w")
  (replace-re "\\<andw" "and.w")
  (replace-re "\\<cmpw" "cmp.w")
  (replace-re "\\<movw" "mov.w")
  (replace-re "\\<movb" "mov.b")
  (replace-re "\\<lsll" "lsl.l")
  (replace-re "\\<clrl" "clr.l")
  (replace-re "\\<jls" "bls")
  (replace-re "\\<jlt" "blt")
  (replace-re "\\<jvs" "bvs")
  (replace-re "\\<jpl" "bpl.w")
  (replace-re "\\<cmpmb" "cmpm.b")
  (replace-re "\\<orb" "or.b")
  (replace-re "\\<trapls" "tls")
  (replace-re "\\<lsrl" "lsr.l")
  (replace-re "\\<extl" "ext.l")
  (replace-re "\\<eorl" "eor.l")
  (replace-re "\\<negl" "neg.l")
  (replace-re "\\<negw" "neg.w")
  (replace-re "\\<mulsl" "muls.l")
  (replace-re "\\<divsl" "divs.l")
  (replace-re "\\<divs.ll" "divsl.l")
  (replace-re "\\<asrl" "asr.l")
  (replace-re "\\<addl" "add.l")
  (replace-re "\\<addw" "add.w")
  (replace-re "\\<addql" "addq.l")
  (replace-re "\\<andb" "and.b")
  (replace-re "\\<andil" "andi.l")
  (replace-re "\\<andl" "and.l")
  (replace-re "\\<asll" "asl.l")
  (replace-re "\\<asrl" "asr.l")
  (replace-re "\\<clrl" "clr.l")
  (replace-re "\\<cmpl" "cmp.l")
  (replace-re "\\<cmpml" "cmpm.l")
  (replace-re "\\<fatand" "fatan.d")
  (replace-re "\\<fcmpl" "fcmp.l")
  (replace-re "\\<fcmpx" "fcmp.x")
  (replace-re "\\<fcosd" "fcos.d")
  (replace-re "\\<fetoxd" "fetox.d")
  (replace-re "\\<fgetexpd" "fgetexp.d")
  (replace-re "\\<fintrzx" "fintrz.x")
  (replace-re "\\<fjeq" "fbeq")
  (replace-re "\\<flognd" "flogn.d")
  (replace-re "\\<fmoved" "fmove.d")
  (replace-re "\\<fmovel" "fmove.l")
  (replace-re "\\<fsind" "fsin.d")
  (replace-re "\\<fsqrtd" "fsqrt.d")
  (replace-re "\\<ftstx" "ftest.x")
  (replace-re "\\<flognx" "flogn.x")
  (replace-re "\\<fsqrtx" "fsqrt.x")
  (replace-re "\\<jbsr" "jsr")
  (replace-re "\\<jcc" "bcc.w")
  (replace-re "\\<jcs" "bcs.w")
  (replace-re "\\<jeq" "beq.w")
  (replace-re "\\<jge" "bge.w")
  (replace-re "\\<jgt" "bgt.w")
  (replace-re "\\<jle" "ble.w")
  (replace-re "\\<jne" "bne.w")
  (replace-re "\\<jra" "bra.w")
  (replace-re "\\<jmi" "bmi.w")
  (replace-re "\\<moveml" "movem.l")
  (replace-re "\\<moveq" "movq.l")
  (replace-re "\\<movl" "mov.l")
  (replace-re "\\<orl" "or.l")
  (replace-re "\\<subl" "sub.l")
  (replace-re "\\<subql" "subq.l")
  (replace-re "\\<tstl" "tst.l")
  (replace-re "\\<trapmi" "tmi")
  (replace-re "(%\\(..\\))-" "-(%\\1)")
  (replace-re "\\<cmp.l[\t ]*\\([^,]*\\),%a\\(.\\)" "cmpa.l %a\\2,\\1")
  (replace-re "\\<cmp.l[\t ]*\\([^,]*\\),%d\\(.\\)" "cmp.l %d\\2,\\1")
  (replace-re "\\<cmp.w[\t ]*\\([^,]*\\),%d\\(.\\)" "cmp.w %d\\2,\\1")
  (replace-re "\\<fcmp.x[\t ]*\\([^,]*\\),%fp\\(.\\)" "fcmp.x %fp\\2,\\1")
)


;; do-convert -- the main conversion routine. Converts the file named by the
;; first arg into the file named by the second.
;;
(defun do-convert (iname oname)
  (find-file iname)
  (setq label-count 0)
  (let ((labels (find-all-label-definitions)))
    (replace-all-references-to-labels labels)
    (replace-all-label-definitions labels))
  (do-subst)
  (write-file oname))


;; now the code that is executed when called in batch mode.
;;
(setq make-backup-files nil)

(do-convert (nth 3 command-line-args)
	    (nth 4 command-line-args))

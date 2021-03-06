;;; The functions in this module are used to emit C code.  At the
;;; current time, the only functions are to collect the code and then print
;;; it out when each block completes.
;;;

;*           Copyright 1989-1993 Digital Equipment Corporation
;*                         All Rights Reserved
;*
;* Permission to use, copy, and modify this software and its documentation is
;* hereby granted only under the following terms and conditions.  Both the
;* above copyright notice and this permission notice must appear in all copies
;* of the software, derivative works or modified versions, and any portions
;* thereof, and both notices must appear in supporting documentation.
;*
;* Users of this software agree to the terms and conditions set forth herein,
;* and hereby grant back to Digital a non-exclusive, unrestricted, royalty-free
;* right and license under any changes, enhancements or extensions made to the
;* core functions of the software, including but not limited to those affording
;* compatibility with other hardware or software environments, but excluding
;* applications which incorporate this software.  Users further agree to use
;* their best efforts to return to Digital any such changes, enhancements or
;* extensions that they make and inform Digital of noteworthy uses of this
;* software.  Correspondence should be provided to Digital at:
;* 
;*                       Director of Licensing
;*                       Western Research Laboratory
;*                       Digital Equipment Corporation
;*                       250 University Avenue
;*                       Palo Alto, California  94301  
;* 
;* This software may be distributed (but not offered for sale or transferred
;* for compensation) to third parties, provided such third parties agree to
;* abide by the terms and conditions of this notice.  
;* 
;* THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
;* WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
;* MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
;* CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;* DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;* PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
;* ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;* SOFTWARE.

(module lap)

;;; Global code is emitted by calling the following function.  As it consists
;;; solely of declarations, no peep-hole optimization need be down.

(define GLOBAL-LAP-CODE '())

(define (EMIT-GLOBAL-LAP code)
    (set! global-lap-code (cons code global-lap-code)))

;;; LAP-CODE is a list of the current generated code.  As items are cons'ed
;;; onto it, it is in reverse order.

(define LAP-CODE '())

;;; CODE is generated by calling EMIT-CODE with a list which consists
;;; of the operator followed by any operands.  At this point, a small
;;; amount of peep-hole optimization is done.

(define (EMIT-LAP code)
    (let* ((old lap-code)
	   (new (peep-lap code)))
	  (if (and (log? 'peep) (not (equal? new (cons code old))))
	      (begin (format sc-icode "/* ")
		     (do ((i (min 1 (- (length old) 1)) (- i 1)))
			 ((negative? i))
			 (format sc-icode " ~A" (list-ref old i)))
		     (format sc-icode " ~A~%  =>" code)
		     (do ((i (min 2 (- (length new) 1)) (- i 1)))
			 ((negative? i))
			 (format sc-icode " ~A" (list-ref new i)))
		     (format sc-icode " */~%")))))	     

(define (PEEP-LAP code)
    (cond ((null? lap-code)
	   (set! lap-code (cons code lap-code)))
	  ((eq? (car code) 'LABEL)
	   (cond ((eq? (caar lap-code) 'LABEL)
		  ; L1        =>     L1
		  ; L2
		  (let ((l1 (cadar lap-code))
			(l2 (cadr code)))
		       (set-id-alias! l2 l1)
		       (set-id-gotos! (resolve-label l2)
		                      (+ (id-gotos (resolve-label l2))
					 (id-gotos l2)))
		       (set! code (car lap-code))
		       (set! lap-code (cdr lap-code))
		       (emit-lap code)))
		 ((equal? (car lap-code) (list 'goto (cadr code)))
		  ; GOTO L    =>     L
		  ; L
		  (bump-label-gotos (car lap-code) -1)
		  (set! lap-code (cdr lap-code))
		  (emit-lap code))
		 ((and (eq? (caar lap-code) 'goto) (eq? (caadr lap-code) 'if)
		       (eq? (resolve-label (caddr (cadr lap-code)))
			    (resolve-label (cadr code))))
		  ; IF TEST GOTO L1   =>  IF NOT TEST GOTO L2
		  ; GOTO L2               L1
		  ; L1
		  (let* ((test (cadadr lap-code))
			 (op   (and (pair? test) (car test))))
			(bump-label-gotos (cadr lap-code) -1)
			(set! test
			      (if (and (pair? test) (eq? (car test) 'NOT))
				  (cadr test)
				  `(NOT ,test)))
			(set! lap-code
			      (cons code
				    (cons (list 'if test (cadar lap-code))
					  (cddr lap-code))))))
		 (else  (set! lap-code (cons code lap-code)))))
	  ((and (eq? (car code) 'SET)
		(or (and (eq? (cadr code) 'no-value)
			 (and (not (pair? (caddr code)))
			      (not (eq? (caddr code) 'tos))))
		    (eq? (caddr code) 'no-value)
		    (equal? (cadr code) (caddr code))))
		; no-value := x      =>
		; x := no-value      =>
	        ; x := x             =>
	     ; Flush Loads or Stores which are "nop's".
	   #t)
	  ((and lap-code
		(or (eq? (caar lap-code) 'goto)
		    (and (eq? (caar lap-code) 'set)
			 (eq? (cadar lap-code) 'return)))
		(not (memq (car code) '(LIT INDENT LABEL))))
	        ; GOTO L / RETURN		=>   GOTO L / RETURN
		; << anything but a label or end >>
	   #t)
	  ((and (eq? (car code) 'goto) (eq? (caar lap-code) 'label)
		(not (eq? (resolve-label (cadr code)) (cadar lap-code))))
	        ; L1		  =>   GOTO L2   (maybe!)
		; GOTO L2
	   (set-id-alias! (cadar lap-code) (resolve-label (cadr code)))
           (set-id-gotos! (resolve-label (cadr code))
	       (+ (id-gotos (resolve-label (cadr code)))
	 	  (id-gotos (cadar lap-code))))
	   (set! lap-code (cdr lap-code))
	   (emit-lap code))
	  (else
	   (bump-label-gotos code 1)
	   (set! lap-code (cons code lap-code))))
    lap-code)

(define (BUMP-LABEL-GOTOS lap incdec)
    (let ((label (case (car lap)
		       ((if) (caddr lap))
		       ((goto) (cadr lap))
		       (else #f))))
	 (if label
	     (begin (set! label (resolve-label label))
		    (set-id-gotos! label (+ (id-gotos label) incdec))))))

(define (SAVE-CURRENT-LAP lap)
    (let ((result (list lap-code lap-temps-used lap-temps-free
			lap-max-display)))
	 (if lap
	     (begin (set! lap-code (list-ref lap 0))
		    (set! lap-temps-used (list-ref lap 1))
		    (set! lap-temps-free (list-ref lap 2))
		    (set! lap-max-display (list-ref lap 3)))
	     (begin (set! lap-code '())
		    (set! lap-temps-used '())
		    (set! lap-temps-free
			  '(       X1   X2   X3   X4   X5   X6   X7   X8   X9
			     X10  X11  X12  X13  X14  X15  X16  X17  X18  X19
			     X20  X21  X22  X23  X24  X25  X26  X27  X28  X29
			     X30  X31  X32  X33  X34  X35  X36  X37  X38  X39
			     X40  X41  X42  X43  X44  X45  X46  X47  X48  X49
			     X50  X51  X52  X53  X54  X55  X56  X57  X58  X59
			     X60  X61  X62  X63  X64  X65  X66  X67  X68  X69
			     X70  X71  X72  X73  X74  X75  X76  X77  X78  X79
			     X80  X81  X82  X83  X84  X85  X86  X87  X88  X89
			     X90  X91  X92  X93  X94  X95  X96  X97  X98  X99
			    X100 X101 X102 X103 X104 X105 X106 X107 X108 X109
			    X110 X111 X112 X113 X114 X115 X116 X117 X118 X119
			    X120 X121 X122 X123 X124 X125 X126 X127 X128 X129
			    X130 X131 X132 X133 X134 X135 X136 X137 X138 X139
			    X140 X141 X142 X143 X144 X145 X146 X147 X148 X149
			    X150 X151 X152 X153 X154 X155 X156 X157 X158 X159
			    X160 X161 X162 X163 X164 X165 X166 X167 X168 X169
			    X170 X171 X172 X173 X174 X175 X176 X177 X178 X179
			    X180 X181 X182 X183 X184 X185 X186 X187 X188 X189
			    X190 X191 X192 X193 X194 X195 X196 X197 X198 X199
			    X200 X201 X202 X203 X204 X205 X206 X207 X208 X209
			    X210 X211 X212 X213 X214 X215 X216 X217 X218 X219
			    X220 X221 X222 X223 X224 X225 X226 X227 X228 X229
			    X230 X231 X232 X233 X234 X235 X236 X237 X238 X239
			    X240 X241 X242 X243 X244 X245 X246 X247 X248 X249
			    X250 X251 X252 X253 X254 X255 X256 X257 X258 X259
			    X260 X261 X262 X263 X264 X265 X266 X267 X268 X269
			    X270 X271 X272 X273 X274 X275 X276 X277 X278 X279
			    X280 X281 X282 X283 X284 X285 X286 X287 X288 X289
			    X290 X291 X292 X293 X294 X295 X296 X297 X298 X299))
		    (set! lap-max-display free-display)))
	 result))

(define LAP-TEMPS-USED '())

(define LAP-TEMPS-FREE '())

(define LAP-MAX-DISPLAY 0)

(define (USE-LAP-TEMP)
    (if (null? lap-temps-free)
	(begin (report-error "Procedure required >= 300 temporary variables")
	       'x299)
	(let ((temp (car lap-temps-free)))
	     (if (not (memq temp lap-temps-used))
		 (set! lap-temps-used (cons temp lap-temps-used)))
	     (set! lap-temps-free (cdr lap-temps-free))
	     temp)))

(define (DROP-LAP-TEMP temp)
    (set! lap-temps-free (cons temp lap-temps-free)))

(define (SAVE-LAP-TEMPS) lap-temps-free)

(define (RESTORE-LAP-TEMPS state) (set! lap-temps-free state))

(define DONE-LAP-LAP '())

(define (DONE-LAP lap)
    (if global-lap-code
	(let ((lap (list global-lap-code '() '() '())))
	     (set! global-lap-code '())
	     (done-lap lap)))
    (if (log? 'lap)
	(begin (format sc-icode "/* ")
	       (pretty-print-$tree (reverse (car lap)) sc-icode)
	       (format sc-icode " */~%")))
    (set! done-lap-lap lap)
    (pplap (reverse (car lap)) (cadr lap) (cadddr lap) sc-icode))

(define (RESOLVE-LABEL label)
    (let ((new (id-alias label)))
	 (if new
	     (resolve-label new)
	     label)))

(define (PPLAP laps temps lap-max-display port)
    (let ((indent "")
	  (display-base #f)
	  (emitted-labels '()))

	 (define (PRINT-GOTO prefix label)
		 (if (memq label emitted-labels)
		     (format port "~aGOBACK( ~a );~%" prefix label)
		     (format port "~agoto ~a;~%" prefix label)))

	 (newline port)
	 (set! pplap-tos #f)
	 (for-each
	     (lambda (lap)
		     (case (car lap)
			   ((LIT)
			    (display indent port)
			    (for-each
				(lambda (x)
					(if (pair? x)
					    (pplap-call x port)
					    (display x port))) (cdr lap))
			    (newline port))
			   ((LABEL)
			    (let ((label (resolve-label (cadr lap))))
				 (when (and (eq? label (cadr lap))
					    (not (zero? (id-gotos label))))
				       (set! emitted-labels
					     (cons label emitted-labels))
			               (format port "~a:~%" label))))
			   ((INDENT)
			    (set! indent (make-string (cadr lap) #\space)))
			   ((PROC)
			    (display "TSCP  " port)
			    (pplap-call (cdr lap) port)
			    (newline port)
			    (when (cddr lap)
				  (display "        TSCP  " port)
				  (pplap-comma-list (cddr lap) port)
				  (display ";" port)
				  (newline port))) 
			   ((LOCALS)
			    (when temps
				  (format port "~aTSCP  " indent)
				  (pplap-comma-list (map vname temps) port)
				  (format port ";~%"))
			    (if (cdr lap)
				(let ((base (caddr lap)))
				     (let loop ((x base))
					  (when (< x lap-max-display)
						(format port
					    "~aTSCP  SD~a = DISPLAY( ~a );~%"
					                indent x x)
						(loop (+ x 1))))
				     (if (not (= base lap-max-display))
					 (begin (set! display-base base)
						(format port
							"~aTSCP  SDVAL;~%"
							indent)))))
			    (if (or temps display-base) (newline port)))
			   ((GOTO)
			    (print-goto indent (resolve-label (cadr lap))))
			   ((IF)
			    (format port "~aif  ( " indent)
			    (pplap-call (cadr lap) port)
			    (if (< (- (write-width port) (write-count port))
				   20)
				(format port "~%~a   " indent))
			    (print-goto " )  " (resolve-label (caddr lap))))
			   ((SET)
			    (case (cadr lap)
				  ((NO-VALUE)
				   (display indent port)
				   (pplap-call (caddr lap) port)
				   (format port ";~%"))
				  ((TOS)
				   (let ((new-tos (subst-tos (caddr lap))))
					(if pplap-tos
				            (report-error
						"PPLAP compiler error"))
					(set! pplap-tos new-tos)))
				  ((RETURN)
				   (when display-base
					 (unless (equal? (caddr lap) "void")
					      (format port "~aSDVAL = " indent)
					      (pplap-call (caddr lap) port)
					      (format port ";~%"))
				         (let loop ((x display-base))
					      (when (< x lap-max-display)
						    (format port
						    "~aDISPLAY( ~a ) = SD~a;~%"
						            indent x x)
						    (loop (+ x 1)))))
				   (let ((val (if display-base
						  '("SDVAL")
						  (cddr lap))))
				        (display indent port)
					(cond ((equal? (caddr lap) "void")
					       (display "return" port))
					      (sc-stack-trace
				               (pplap-call
						   (cons "POPSTACKTRACE" val)
						   port))
					      (else
					       (pplap-call
						   (cons "return" val)
					           port))))
				   (format port ";~%"))
				  (else
				   (display indent port)
				   (pplap-call (cadr lap) port)
				   (display " = " port)
				   (pplap-call (caddr lap) port)
				   (format port ";~%"))))
			   (else  (display indent port)
				  (pplap-call lap port)
				  (format port ";~%")))) 
	     laps)))

(define PPLAP-TOS '())

(define (POP-TOS)
    (let ((tos pplap-tos))
	 (set! pplap-tos #f)
	 (if tos tos (report-error "POP-TOS compiler error"))))

(define (SUBST-TOS form)
    (cond ((eq? form 'tos)
	   (pop-tos))
	  ((pair? form)
	   (cons (subst-tos (car form)) (subst-tos (cdr form))))
	  (else form)))

(define (PPLAP-CALL lap port)
    (let ((limit (- (write-width port) 5)))
	 (cond ((pair? lap)
	        (cond ((eq? (car lap) 'CSTRING)
		       (display #\" port)
		       (for-each
			   (lambda (c)
				   (cond ((assq c '((#\tab . #\t)
						    (#\newline . #\n)
						    (#\linefeed . #\n)
						    (#\formfeed . #\f)
						    (#\return . #\r)
						    (#\" . #\")
						    (#\\ . #\\)))
					   => (lambda (old.new)
						      (display #\\ port)
						      (display (cdr old.new)
							  port)))
					 ((or (char<? c #\space)
					      (char>? c #\~))
					  (display #\\ port)
					  (display (list->string
						       (char->dl c 8 3)) port))
					 (else (display c port)))
				   (when (> (write-count port) limit)
				         (display #\\ port)
				         (newline port)))
			   (string->list (cadr lap)))
		       (display #\" port))
		      ((and (memq (car lap) '(TRUE FALSE NOT))
			    (eq? (cadr lap) 'TOS))
		       (pplap-call (list (car lap) (pop-tos)) port))
		      ((and (eq? (car lap) 'NOT) (pair? (cadr lap)))
		       (let* ((op (caadr lap))
			      (operands (cdadr lap))
			      (invert (assq op
					   '((EQ . NEQ)
					     (NEQ . EQ)
					     (TRUE . FALSE)
					     (FALSE . TRUE)
					     (LT . GTE)
					     (GTE . LT)
					     (GT . LTE)
					     (LTE . GT)))))
			     (cond ((eq? op 'NOT)
				    (pplap-call (car operands) port))
				   (invert
				    (pplap-call (cons (cdr invert) operands)
					port))
				   (else (format port "~a( " (car lap))
					 (pplap-comma-list (cdr lap) port)
					 (display " )" port)))))
		      ((and (eq? (car lap) 'TRUE) (pair? (cadr lap))
			    (eq? (caadr lap) 'BOOLEAN))
		       (pplap-call (cadadr lap) port))
		      ((and (eq? (car lap) 'FALSE) (pair? (cadr lap))
			    (eq? (caadr lap) 'BOOLEAN))
		       (pplap-call `(NOT ,(cadadr lap)) port))
		      (else (pplap-call (car lap) port)
		            (display "( " port)
			    (pplap-comma-list (cdr lap) port)
			    (display " )" port))))
	        ((eq? lap 'TOS)
		 (pplap-call (pop-tos) port))
		((and (symbol? lap) (id-use lap))
		 (if (not (eq? (vname lap) lap))
		     (report-error "PPLAP looked up a symbol:" lap))
		 (display (vname lap) port))
		(else (display lap port)))))

(define (PPLAP-COMMA-LIST lap port)
    (let* ((indent (write-count port))
	   (nextline (negative? (pplap-size lap
				    (- (write-width port) indent)))))
	  (when lap
		(pplap-call (car lap) port)
		(when (cdr lap)
		      (display ", " port)
		      (when nextline
			    (newline port)
			    (set-write-count! port indent))
		      (pplap-comma-list (cdr lap) port)))))

(define (PPLAP-SIZE lap left)
    (cond ((negative? left) left)
	  ((null? lap) left)
	  ((pair? lap)
	   (if (eq? (car lap) 'CSTRING)
	       (- left (+ (string-length (cadr lap)) 5))
	       (pplap-size (cdr lap) (pplap-size (car lap) (- left 4)))))
	  ((eq? lap 'TOS)
	   (pplap-size pplap-tos left))
	  (else (- left
		   (string-length (format "~a.."
					  (if (and (symbol? lap) (id-use lap))
					      (vname lap)
					      lap)))))))

;;; Downshift a symbol name.  Leave any other value unchanged.

(define (DOWNSHIFT op)
    (if (symbol? op) (string-downcase (symbol->string op)) op))

;;; Initialization for this module is preformed by the following procedure.

(define (LOAD-PLIST-LAP)
    (for-each
	(lambda (x)
		(set-id-vname! x x)
		(set-id-use! x '$lexical))
	'(        X1   X2   X3   X4   X5   X6   X7   X8   X9
	     X10  X11  X12  X13  X14  X15  X16  X17  X18  X19
	     X20  X21  X22  X23  X24  X25  X26  X27  X28  X29
	     X30  X31  X32  X33  X34  X35  X36  X37  X38  X39
	     X40  X41  X42  X43  X44  X45  X46  X47  X48  X49
	     X50  X51  X52  X53  X54  X55  X56  X57  X58  X59
	     X60  X61  X62  X63  X64  X65  X66  X67  X68  X69
	     X70  X71  X72  X73  X74  X75  X76  X77  X78  X79
	     X80  X81  X82  X83  X84  X85  X86  X87  X88  X89
	     X90  X91  X92  X93  X94  X95  X96  X97  X98  X99
	     X100 X101 X102 X103 X104 X105 X106 X107 X108 X109
	     X110 X111 X112 X113 X114 X115 X116 X117 X118 X119
	     X120 X121 X122 X123 X124 X125 X126 X127 X128 X129
	     X130 X131 X132 X133 X134 X135 X136 X137 X138 X139
	     X140 X141 X142 X143 X144 X145 X146 X147 X148 X149
	     X150 X151 X152 X153 X154 X155 X156 X157 X158 X159
	     X160 X161 X162 X163 X164 X165 X166 X167 X168 X169
	     X170 X171 X172 X173 X174 X175 X176 X177 X178 X179
	     X180 X181 X182 X183 X184 X185 X186 X187 X188 X189
	     X190 X191 X192 X193 X194 X195 X196 X197 X198 X199
	     X200 X201 X202 X203 X204 X205 X206 X207 X208 X209
	     X210 X211 X212 X213 X214 X215 X216 X217 X218 X219
	     X220 X221 X222 X223 X224 X225 X226 X227 X228 X229
	     X230 X231 X232 X233 X234 X235 X236 X237 X238 X239
	     X240 X241 X242 X243 X244 X245 X246 X247 X248 X249
	     X250 X251 X252 X253 X254 X255 X256 X257 X258 X259
	     X260 X261 X262 X263 X264 X265 X266 X267 X268 X269
	     X270 X271 X272 X273 X274 X275 X276 X277 X278 X279
	     X280 X281 X282 X283 X284 X285 X286 X287 X288 X289
	     X290 X291 X292 X293 X294 X295 X296 X297 X298 X299
	     TOS NO-VALUE RETURN)))

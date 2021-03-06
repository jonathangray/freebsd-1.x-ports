;;;
;;; Scheme->C test program
;;;
;;;
;;; Test functions for basic Scheme functions.
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

(module test14)

(define-external (chk testnum result expected) testchk)

(define (RR s)
    (set! %record-prefix-char #\~)
    (set! %record-read (lambda (port) (list->%record (read port))))
    (read (open-input-string s))) 

(define (test14)
    
    ;;; 6.8  Vectors
    
    (chk 01 (vector? '()) #f)
    (chk 02 (vector? '#()) #t)
    (chk 03 (vector? '(1 2)) #f)
    (chk 04 (vector? '#(1 2)) #t)
    (chk 05 (vector? 'x) #f)
    (chk 06 (vector? "x") #f)
    (chk 07 (vector? '#\a) #f)
    (chk 08 (vector? (lambda (x) x)) #f)
    (chk 09 (vector? #f) #f)
    (chk 10 (vector? #t) #f)
    (chk 11 (vector? -1) #f)
    (chk 12 (vector? 0) #f)
    (chk 13 (vector? 1) #f)
    (chk 14 (vector? -2.5) #f)
    (chk 15 (vector? 0.0) #f)
    (chk 16 (vector? 1.5) #f)
    
    (chk 20 (vector-length (make-vector 20)) 20)
    (chk 21 (eq? (make-vector 0) '#()) #t)
    (chk 22 (make-vector 5 (make-vector 1 1)) '#(#(1) #(1) #(1) #(1) #(1)))
    
    (chk 30 (vector) '#())
    (chk 31 (vector 0) '#(0))
    (chk 32 (vector 0 1) '#(0 1))
    (chk 31 (vector 0 1 2 3 4) '#(0 1 2 3 4))
    
    (chk 40 (vector-length '#()) 0)
    (chk 41 (vector-length '#(1 2 3)) 3)
    (chk 42 (vector-length (make-vector 1000)) 1000)
    
    (chk 50 (vector-ref '#(zero one two three) 0) 'zero)
    (chk 51 (vector-ref '#(zero one two three) 1) 'one)
    (chk 52 (vector-ref '#(zero one two three) 3) 'three)
    
    (let ((xvector (make-vector 4)))
	 (vector-set! xvector 0 'zero)
	 (vector-set! xvector 1 'one)
	 (vector-set! xvector 2 'two)
	 (vector-set! xvector 3 'three)
	 (chk 60 xvector '#(zero one two three)))
    
    (chk 70 (vector->list '#()) '())  
    (chk 71 (vector->list '#(zero one two three)) '(zero one two three))
    
    (chk 80 (list->vector '()) '#())
    (chk 81 (list->vector '(1)) '#(1))
    (chk 82 (list->vector '(1 2 3 4)) '#(1 2 3 4))
    
    (chk 90 (vector-fill! (make-vector 10) #t)
	 '#(#t #t #t #t #t #t #t #t #t #t))
    (chk 91 (vector-fill! '#() 1) '#())
    
    ;;; *.*  Records
    
    (chk 101 (%record? '()) #f)
    (chk 102 (%record? '#()) #f)
    (chk 103 (%record? '(1 2)) #f)
    (chk 104 (%record? '#(1 2)) #f)
    (chk 105 (%record? 'x) #f)
    (chk 106 (%record? "x") #f)
    (chk 107 (%record? '#\a) #f)
    (chk 108 (%record? (lambda (x) x)) #f)
    (chk 109 (%record? #f) #f)
    (chk 110 (%record? #t) #f)
    (chk 111 (%record? -1) #f)
    (chk 112 (%record? 0) #f)
    (chk 113 (%record? 1) #f)
    (chk 114 (%record? -2.5) #f)
    (chk 115 (%record? 0.0) #f)
    (chk 116 (%record? 1.5) #f)
    (chk 117 (%record? (make-%record 1)) #t)
    
    (chk 120 (%record-length (make-%record 20)) 20)
    (chk 121 (eq? (make-%record 0) (make-%record 0)) #f)
    (chk 122 (eq? (make-%record 0) (make-%record 0)) #f)
    (chk 123 (equal? (make-%record 0) (make-%record 0)) #f)
    (chk 124 (make-%record 0) (make-%record 0))
    (chk 125 (make-%record 5 #t) (rr "#~(#t #t #t #t #t)"))
    
    (chk 130 (%record) (rr "#~()"))
    (chk 131 (%record 0) (rr "#~(0)"))
    (chk 132 (%record 0 1) (rr "#~(0 1)"))
    (chk 131 (%record 0 1 2 3 4) (rr "#~(0 1 2 3 4)"))
    
    (chk 140 (%record-length (rr "#~()")) 0)
    (chk 141 (%record-length (rr "#~(1 2 3)")) 3)
    (chk 142 (%record-length (make-%record 1000)) 1000)
    
    (chk 150 (%record-ref (rr "#~(zero one two three)") 0) 'zero)
    (chk 151 (%record-ref (rr "#~(zero one two three)") 1) 'one)
    (chk 152 (%record-ref (rr "#~(zero one two three)") 3) 'three)
    
    (let ((x%record (make-%record 4)))
	 (%record-set! x%record 0 'zero)
	 (%record-set! x%record 1 'one)
	 (%record-set! x%record 2 'two)
	 (%record-set! x%record 3 'three)
	 (chk 160 x%record (rr "#~(zero one two three)")))
    
    (chk 170 (%record->list (rr "#~()")) '()) 
    (chk 171 (%record->list (rr "#~(zero one two three)"))
	 '(zero one two three))
    
    (chk 180 (list->%record '()) (rr "#~()"))
    (chk 181 (list->%record '(1)) (rr "#~(1)"))
    (chk 182 (list->%record '(1 2 3 4)) (rr "#~(1 2 3 4)")))

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

(module test12)

(define-external (chk testnum result expected) testchk)

(define (test12)

;;; 6.6  Characters
     
    (chk 1 (char? '()) #f)
    (chk 2 (char? '#()) #f)
    (chk 3 (char? '(1 2)) #f)
    (chk 4 (char? '#(1 2)) #f)
    (chk 5 (char? 'x) #f)
    (chk 6 (char? "x") #f)
    (chk 7 (char? #\a) #t)
    (chk 8 (char? (lambda (x) x)) #f)
    (chk 9 (char? #f) #f)
    (chk 10 (char? #t) #f)
    (chk 11 (char? -1) #f)
    (chk 12 (char? 0) #f)
    (chk 13 (char? 1) #f)
    (chk 14 (char? -2.5) #f)
    (chk 15 (char? 0.0) #f)
    (chk 16 (char? 1.5) #f)

    (chk 20 (char=? #\a #\A) #f)
    (chk 21 (char=? #\2 #\4) #f)
    (chk 22 (char=? #\a #\b) #f)
    (chk 23 (char=? #\b #\a) #f)
    (chk 24 (char=? #\c #\c) #t)

    (chk 30 (char<? #\a #\A) #f)
    (chk 31 (char<? #\2 #\4) #t)
    (chk 32 (char<? #\a #\b) #t)
    (chk 33 (char<? #\b #\a) #f)
    (chk 34 (char<? #\c #\c) #f)

    (chk 40 (char>? #\a #\A) #t)
    (chk 41 (char>? #\2 #\4) #f)
    (chk 42 (char>? #\a #\b) #f)
    (chk 43 (char>? #\b #\a) #t)
    (chk 44 (char>? #\c #\c) #f)

    (chk 50 (char<=? #\a #\A) #f)
    (chk 51 (char<=? #\2 #\4) #t)
    (chk 52 (char<=? #\a #\b) #t)
    (chk 53 (char<=? #\b #\a) #f)
    (chk 54 (char<=? #\c #\c) #t)

    (chk 60 (char>=? #\a #\A) #t)
    (chk 61 (char>=? #\2 #\4) #f)
    (chk 62 (char>=? #\a #\b) #f)
    (chk 63 (char>=? #\b #\a) #t)
    (chk 64 (char>=? #\c #\c) #t)

    (chk 70 (char-alphabetic? #\.) #f)
    (chk 71 (char-alphabetic? #\3) #f)
    (chk 72 (char-alphabetic? #\a) #t)
    (chk 73 (char-alphabetic? #\A) #t)
    (chk 74 (char-alphabetic? #\tab) #f)
    (chk 75 (char-alphabetic? #\space) #f)
    (chk 76 (char-alphabetic? #\newline) #f)

    (chk 80 (char-numeric? #\.) #f)
    (chk 81 (char-numeric? #\3) #t)
    (chk 82 (char-numeric? #\a) #f)
    (chk 83 (char-numeric? #\A) #f)
    (chk 84 (char-numeric? #\tab) #f)
    (chk 85 (char-numeric? #\space) #f)
    (chk 86 (char-numeric? #\newline) #f)

    (chk 90 (char-whitespace? #\.) #f)
    (chk 91 (char-whitespace? #\3) #f)
    (chk 92 (char-whitespace? #\a) #f)
    (chk 93 (char-whitespace? #\A) #f)
    (chk 94 (char-whitespace? #\tab) #t)
    (chk 95 (char-whitespace? #\space) #t)
    (chk 96 (char-whitespace? #\newline) #t)

    (chk 100 (char-upper-case? #\a) #f)
    (chk 101 (char-upper-case? #\A) #t)
    (chk 102 (char-upper-case? #\z) #f)
    (chk 103 (char-upper-case? #\Z) #t)

    (chk 110 (char-lower-case? #\a) #t)
    (chk 111 (char-lower-case? #\A) #f)
    (chk 112 (char-lower-case? #\z) #t)
    (chk 113 (char-lower-case? #\Z) #f)

    (chk 120 (char-upcase #\a) #\A)
    (chk 121 (char-upcase #\A) #\A)
    (chk 122 (char-upcase #\z) #\Z)
    (chk 123 (char-upcase #\Z) #\Z)
    (chk 124 (char-upcase #\space) #\space)

    (chk 130 (char-downcase #\a) #\a)
    (chk 131 (char-downcase #\A) #\a)
    (chk 132 (char-downcase #\z) #\z)
    (chk 133 (char-downcase #\Z) #\z)
    (chk 134 (char-downcase #\space) #\space)

    (chk 140 (char->integer #\space) 32)
    (chk 141 (char->integer #\A) 65)

    (chk 150 (integer->char 32) #\space)
    (chk 151 (integer->char 65) #\A)

    (chk 160 (char-ci=? #\a #\A) #t)
    (chk 161 (char-ci=? #\2 #\4) #f)
    (chk 162 (char-ci=? #\a #\b) #f)
    (chk 163 (char-ci=? #\b #\a) #f)
    (chk 164 (char-ci=? #\c #\c) #t)
    (chk 165 (char-ci=? #\C #\c) #t)

    (chk 170 (char-ci<? #\a #\A) #f)
    (chk 171 (char-ci<? #\2 #\4) #t)
    (chk 172 (char-ci<? #\a #\b) #t)
    (chk 173 (char-ci<? #\b #\a) #f)
    (chk 174 (char-ci<? #\c #\c) #f)
    (chk 175 (char-ci<? #\a #\B) #t)
    (chk 176 (char-ci<? #\A #\b) #t)
    (chk 177 (char-ci<? #\B #\a) #f)

    (chk 180 (char-ci>? #\a #\A) #f)
    (chk 181 (char-ci>? #\2 #\4) #f)
    (chk 182 (char-ci>? #\a #\b) #f)
    (chk 183 (char-ci>? #\b #\a) #t)
    (chk 184 (char-ci>? #\B #\a) #t)
    (chk 185 (char-ci>? #\b #\A) #t)
    (chk 186 (char-ci>? #\a #\B) #f)
    (chk 187 (char-ci>? #\c #\c) #f)

    (chk 190 (char-ci<=? #\a #\A) #t)
    (chk 191 (char-ci<=? #\2 #\4) #t)
    (chk 192 (char-ci<=? #\a #\b) #t)
    (chk 193 (char-ci<=? #\b #\a) #f)
    (chk 194 (char-ci<=? #\c #\c) #t)
    (chk 195 (char-ci<=? #\c #\D) #t)
    (chk 196 (char-ci<=? #\C #\a) #f)
    (chk 197 (char-ci<=? #\a #\1) #f)

    (chk 200 (char-ci>=? #\a #\A) #t)
    (chk 201 (char-ci>=? #\2 #\4) #f)
    (chk 202 (char-ci>=? #\a #\b) #f)
    (chk 203 (char-ci>=? #\b #\a) #t)
    (chk 204 (char-ci>=? #\C #\c) #t)
    (chk 205 (char-ci>=? #\c #\C) #t)
    (chk 206 (char-ci>=? #\C #\C) #t)
    (chk 207 (char-ci>=? #\c #\D) #f))

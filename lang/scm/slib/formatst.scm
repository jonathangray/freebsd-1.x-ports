;;
;; FORMAT Version 2.3 conformance test
;;
;; Test run: (load "format.scm") (load "formatst.scm")
;;
(require 'format)
;
; Failure reports for various scheme interpreters:
;
; SCM4b,c/SLIB:
;   None.
; Elk 1.5/2.0:
;   7 inconsistencies due to the use of custom types (so these are no errors).
; MIT C-Scheme 7.1:
;   The empty list is always evaluated as a boolean and consequently
;   represented as `#f'. 6 more inconsistencies due to the use of the
;   custom types (so these are no errors).
; UMB Scheme 2.5/2.10:
;   A `\' is missing in slashified 8bit characters and one error due
;   to the dot exponential number->string representation:
;   (number->string 1e20) -> 1e+20.0 (This should be fixed).
; Scheme->C 01nov91:
;   symbols are generally converted to uppercase strings.
;   number to string conversions have always a number prefix.


(if (not (string=? format:version "2.3"))
    (begin
      (display "CAUTION: you are testing format version ")
      (display format:version)
      (display ". This test is for format version 2.3!")
      (newline)))

(define fails 0)
(define total 0)
(define show-test #f)			; set this to #t if you like

(define (test format-args out-str)
  (set! total (+ total 1))
  (if (not show-test)
      (if (zero? (modulo total 10))
          (begin
            (display total)
            (display ",")
	    (format:force-output (current-output-port)))))
  (let ((format-out (apply format `(#f ,@format-args))))
    (if (string=? out-str format-out)
	(if show-test
	    (begin
	      (display "Verified ")
	      (write format-args)
	      (display " returns ")
	      (write out-str)
	      (newline)))
	(begin
	  (set! fails (+ fails 1))
	  (if (not show-test) (newline))
	  (display "*Failed* ")
	  (write format-args)
	  (display " returns ")
	  (write format-out)
	  (display " instead of ")
	  (write out-str)
	  (newline)))))

; any object test

(test '("abc") "abc")
(test '("~a" 10) "10")
(test '("~a" -1.2) "-1.2")
(test '("~a" a) "a")
(test '("~a" #t) "#t")
(test '("~a" #f) "#f")
(test '("~a" "abc") "abc")
(test '("~a" #(1 2 3)) "#(1 2 3)")
(test '("~a" ()) "()")
(test '("~a" (a)) "(a)")
(test '("~a" (a b)) "(a b)")
(test '("~a" (a (b c) d)) "(a (b c) d)")
(test '("~a" (a . b)) "(a . b)")
(test '("~a" (a (b c . d))) "(a (b . (c . d)))") ; this is ugly
(test `("~a" ,display) (iobj-str "procedure" #f))
(test `("~a" ,(current-input-port)) (iobj-str "input-port" #f))
(test `("~a" ,(current-output-port)) (iobj-str "output-port" #f))

; # argument test

(test '("~a ~a" 10 20) "10 20")
(test '("~a abc ~a def" 10 20) "10 abc 20 def")

; numerical test

(test '("~d" 100) "100")
(test '("~x" 100) "64")
(test '("~o" 100) "144")
(test '("~b" 100) "1100100")
(test '("~@d" 100) "#d100")
(test '("~@x" 100) "#x64")
(test '("~@o" 100) "#o144")
(test '("~@b" 100) "#b1100100")
(test '("~10d" 100) "       100")
(test '("~:d" 12345678) "12,345,678")
(test '("~-6d" 12345678) "<45678")
(test '("~10,'*d" 100) "*******100")
(test '("~10,,'|:d" 12345678) "12|345|678")
(test '("~10,,,2:d" 12345678) "12,34,56,78")
(test '("~14,'*,'|,4:@d" 12345678) "***#d1234|5678")
(test '("~10r" 100) "100")
(test '("~2r" 100) "1100100")
(test '("~8r" 100) "144")
(test '("~16r" 100) "64")
(test '("~16,10,'*r" 100) "********64")

; character test

(test '("~c" #\a) "a")
(test '("~@c" #\a) "#\\a")
(test `("~@c" ,(integer->char 32)) "#\\space")
(test `("~@c" ,(integer->char 0)) "#\\nul")
(test `("~@c" ,(integer->char 27)) "#\\esc")
(test `("~@c" ,(integer->char 127)) "#\\del")
(test `("~@c" ,(integer->char 128)) "#\\200")
(test '("~65c") "A")
(test '("~7@c") "#\\bel")

; plural test

(test '("test~p" 1) "test")
(test '("test~p" 2) "tests")
(test '("test~p" 0) "tests")
(test '("tr~@p" 1) "try")
(test '("tr~@p" 2) "tries")
(test '("tr~@p" 0) "tries")
(test '("~a test~:p" 10) "10 tests")
(test '("~a test~:p" 1) "1 test")

; tilde test

(test '("~~~~") "~~")
(test '("~3~") "~~~")

; whitespace character test

(test '("~%") "
")
(test '("~3%") "


")
(test '("~|") "")
(test '("~_~_~_") "   ")
(test '("~3_") "   ")
(test '("~t") "	")
(test '("~3t") "			")

; indirection test

(test '("~a ~? ~a" 10 "~a ~a" (20 30) 40) "10 20 30 40")
(test '("~a ~@? ~a" 10 "~a ~a" 20 30 40) "10 20 30 40")

; minimum field test

(test '("~10a" "abc") "abc       ")
(test '("~10@a" "abc") "       abc")
(test '("~10a" "0123456789abc") "0123456789abc")
(test '("~10@a" "0123456789abc") "0123456789abc")

; maximum field test

(test '("~-10a" "abc") "abc       ")
(test '("~-10a" "0123456789abc") "012345678>")
(test '("~-10a" "0123456789") "0123456789")
(test '("~-10@a" "0123456789abc") "<456789abc")
(test '("~-10@a" "0123456789") "0123456789")
(test '("~-10a" (a (b c (d e) f) g)) "(a (b c (>")

; pad character field test

(test '("~10,,,'*a" "abc") "abc*******")
(test '("~10,,,'Xa" "abc") "abcXXXXXXX")
(test '("~10,,,42a" "abc") "abc*******")
(test '("~10,,,'*@a" "abc") "*******abc")
(test '("~-10,,,'*a" "0123456789abc") "012345678>")
(test '("~10,,3,'*a" "abc") "abc*******")
(test '("~10,,3,'*a" "0123456789abc") "0123456789abc***") ; min. padchar length
(test '("~10,,3,'*@a" "0123456789abc") "***0123456789abc")
(test '("~-10,,3,'*a" "0123456789abc") "012345678>")
(test '("~-10,,3,'*@a" "0123456789abc") "<456789abc")
(test '("~10,99,3,'*a" "abc") "abc*******") ; 2nd parameter has no effect yet

; slashify test

(test '("~s" "abc") "\"abc\"")
(test '("~s" "abc \\ abc") "\"abc \\\\ abc\"")
(test '("~a" "abc \\ abc") "abc \\ abc")
(test '("~s" "abc \" abc") "\"abc \\\" abc\"")
(test '("~a" "abc \" abc") "abc \" abc")
(test '("~s" #\space) "#\\space")
(test '("~s" #\newline) "#\\newline")
(test '("~s" #\tab) "#\\ht")
(test '("~s" #\a) "#\\a")
(test '("~a" (a "b" c)) "(a \"b\" c)")

; read proof test

(test `("~:s" ,display) (iobj-str "procedure" #t))
(test `("~:a" ,display) (iobj-str "procedure" #t))
(test `("~:a" (1 2 ,display)) (string-append "(1 2 "
					     (iobj-str "procedure" #t) ")"))
(test '("~:a" "abc") "abc")

; continuation line test

(test '("abc~
         123") "abc123")
(test '("abc~
123") "abc123")
(test '("abc~
") "abc")
(test '("abc~:
         def") "abc         def")
(test '("abc~@
         def")
"abc
def")

; flush output (can't test it here really)

(test '("abc ~! xyz") "abc  xyz")

; string case conversion

(test '("~a ~(~a~) ~a" "abc" "HELLO WORLD" "xyz") "abc hello world xyz")
(test '("~a ~:(~a~) ~a" "abc" "HELLO WORLD" "xyz") "abc Hello World xyz")
(test '("~a ~@(~a~) ~a" "abc" "HELLO WORLD" "xyz") "abc Hello world xyz")
(test '("~a ~:@(~a~) ~a" "abc" "hello world" "xyz") "abc HELLO WORLD xyz")
(test '("~:@(~a~)" (a b c)) "(A B C)")
(test '("~:@(~x~)" 255) "FF")
(test '("~:@(~p~)" 2) "S")
(test `("~:@(~a~)" ,display) (iobj-str "PROCEDURE" #f))
(test '("~:(~a ~a ~a~) ~a" "abc" "xyz" "123" "world") "Abc Xyz 123 world")

; variable parameter

(test '("~va" 10 "abc") "abc       ")
(test '("~v,,,va" 10 42 "abc") "abc*******")

; number of remaining arguments as parameter

(test '("~#,,,'*@a ~a ~a ~a" 1 1 1 1) "***1 1 1 1")

; argument jumping

(test '("~a ~* ~a" 10 20 30) "10  30")
(test '("~a ~2* ~a" 10 20 30 40) "10  40")
(test '("~a ~:* ~a" 10) "10  10")
(test '("~a ~a ~2:* ~a ~a" 10 20) "10 20  10 20")
(test '("~a ~a ~@* ~a ~a" 10 20) "10 20  10 20")
(test '("~a ~a ~4@* ~a ~a" 10 20 30 40 50 60) "10 20  50 60")

; conditionals

(test '("~[abc~;xyz~]" 0) "abc")
(test '("~[abc~;xyz~]" 1) "xyz")
(test '("~[abc~;xyz~:;456~]" 99) "456")
(test '("~0[abc~;xyz~:;456~]") "abc")
(test '("~1[abc~;xyz~:;456~] ~a" 100) "xyz 100")
(test '("~#[no arg~;~a~;~a and ~a~;~a, ~a and ~a~]") "no arg")
(test '("~#[no arg~;~a~;~a and ~a~;~a, ~a and ~a~]" 10) "10")
(test '("~#[no arg~;~a~;~a and ~a~;~a, ~a and ~a~]" 10 20) "10 and 20")
(test '("~#[no arg~;~a~;~a and ~a~;~a, ~a and ~a~]" 10 20 30) "10, 20 and 30")
(test '("~:[hello~;world~] ~a" #t 10) "world 10")
(test '("~:[hello~;world~] ~a" #f 10) "hello 10")
(test '("~@[~a tests~]" #f) "")
(test '("~@[~a tests~]" 10) "10 tests")
(test '("~@[~a test~:p~] ~a" 10 done) "10 tests done")
(test '("~@[~a test~:p~] ~a" 1 done) "1 test done")
(test '("~@[~a test~:p~] ~a" 0 done) "0 tests done")
(test '("~@[~a test~:p~] ~a" #f done) " done")
(test '("~@[ level = ~d~]~@[ length = ~d~]" #f 5) " length = 5")
(test '("~[abc~;~[4~;5~;6~]~;xyz~]" 0) "abc")   ; nested conditionals (irrghh)
(test '("~[abc~;~[4~;5~;6~]~;xyz~]" 2) "xyz")
(test '("~[abc~;~[4~;5~;6~]~;xyz~]" 1 2) "6")

; iteration

(test '("~{ ~a ~}" (a b c)) " a  b  c ")
(test '("~{ ~a ~}" ()) "")
(test '("~{ ~a ~5,,,'*a~}" (a b c d)) " a b**** c d****")
(test '("~{ ~a,~a ~}" (a 1 b 2 c 3)) " a,1  b,2  c,3 ")
(test '("~2{ ~a,~a ~}" (a 1 b 2 c 3)) " a,1  b,2 ")
(test '("~3{~a ~} ~a" (a b c d e) 100) "a b c  100")
(test '("~0{~a ~} ~a" (a b c d e) 100) " 100")
(test '("~:{ ~a,~a ~}" ((a b) (c d e f) (g h))) " a,b  c,d  g,h ")
(test '("~2:{ ~a,~a ~}" ((a b) (c d e f) (g h))) " a,b  c,d ")
(test '("~@{ ~a,~a ~}" a 1 b 2 c 3) " a,1  b,2  c,3 ")
(test '("~2@{ ~a,~a ~} <~a|~a>" a 1 b 2 c 3) " a,1  b,2  <c|3>")
(test '("~:@{ ~a,~a ~}" (a 1) (b 2) (c 3)) " a,1  b,2  c,3 ")
(test '("~2:@{ ~a,~a ~} ~a" (a 1) (b 2) (c 3)) " a,1  b,2  (c 3)")
(test '("~{~}" "<~a,~a>" (a 1 b 2 c 3)) "<a,1><b,2><c,3>")
(test '("~{ ~a ~{<~a>~}~} ~a" (a (1 2) b (3 4)) 10) " a <1><2> b <3><4> 10")

; up and out

(test '("abc ~^ xyz") "abc ")
(test '("~@(abc ~^ xyz~) ~a" 10) "ABC  xyz 10")
(test '("done. ~^ ~d warning~:p. ~^ ~d error~:p.") "done. ")
(test '("done. ~^ ~d warning~:p. ~^ ~d error~:p." 10) "done.  10 warnings. ")
(test '("done. ~^ ~d warning~:p. ~^ ~d error~:p." 10 1)
      "done.  10 warnings.  1 error.")
(test '("~{ ~a ~^<~a>~} ~a" (a b c d e f) 10) " a <b> c <d> e <f> 10")
(test '("~{ ~a ~^<~a>~} ~a" (a b c d e) 10) " a <b> c <d> e  10")
(test '("abc~0^ xyz") "abc")
(test '("abc~9^ xyz") "abc xyz")
(test '("abc~7,4^ xyz") "abc xyz")
(test '("abc~7,7^ xyz") "abc")
(test '("abc~3,7,9^ xyz") "abc")
(test '("abc~8,7,9^ xyz") "abc xyz")
(test '("abc~3,7,5^ xyz") "abc xyz")

; complexity tests (oh my god, I hardly understand them myself (see CL std))

(define fmt "Items:~#[ none~; ~a~; ~a and ~a~:;~@{~#[~; and~] ~a~^,~}~].")

(test `(,fmt ) "Items: none.")
(test `(,fmt foo) "Items: foo.")
(test `(,fmt foo bar) "Items: foo and bar.")
(test `(,fmt foo bar baz) "Items: foo, bar, and baz.")
(test `(,fmt foo bar baz zok) "Items: foo, bar, baz, and zok.")

; fixed floating points

(test '("~6,2f" 3.14159) "  3.14")
(test '("~6,1f" 3.14159) "   3.1")
(test '("~6,0f" 3.14159) "    3.")
(test '("~5,1f" 0) "  0.0")
(test '("~10,7f" 3.14159) " 3.1415900")
(test '("~10,7f" -3.14159) "-3.1415900")
(test '("~10,7@f" 3.14159) "+3.1415900")
(test '("~6,3f" 0.0) " 0.000")
(test '("~6,4f" 0.007) "0.0070")
(test '("~6,3f" 0.007) " 0.007")
(test '("~6,2f" 0.007) "  0.01")
(test '("~3,2f" 0.007) ".01")
(test '("~3,2f" -0.007) "-.01")
(test '("~6,2,,,'*f" 3.14159) "**3.14")
(test '("~6,3,,'?f" 12345.56789) "??????")
(test '("~6,3f" 12345.6789) "12345.679")
(test '("~,3f" 12345.6789) "12345.679")
(test '("~,3f" 9.9999) "10.000")
(test '("~6f" 23.4) "  23.4")
(test '("~6f" 1234.5) "1234.5")
(test '("~6f" 12345678) "12345678.0")
(test '("~6,,,'?f" 12345678) "??????")
(test '("~6f" 123.56789) "123.57")
(test '("~6f" 123.0) " 123.0")
(test '("~6f" -123.0) "-123.0")
(test '("~6f" 0.0) "   0.0")
(test '("~3f" 3.141) "3.1")
(test '("~2f" 3.141) "3.")
(test '("~1f" 3.141) "3.141")
(test '("~f" 123.56789) "123.56789")
(test '("~f" -314.0) "-314.0")
(test '("~f" 1e4) "10000.0")
(test '("~f" -1.23e10) "-12300000000.0")
(test '("~f" 1e-4) "0.0001")
(test '("~f" -1.23e-10) "-0.000000000123")
(test '("~@f" 314.0) "+314.0")
(test '("~,,3f" 0.123456) "123.456")
(test '("~,,-3f" -123.456) "-0.123456")

; exponent floating points

(test '("~e" 3.14159) "3.14159E+0")
(test '("~e" 0.00001234) "1.234E-5")
(test '("~,,,0e" 0.00001234) "0.1234E-4")
(test '("~,3e" 3.14159) "3.142E+0")
(test '("~,3@e" 3.14159) "+3.142E+0")
(test '("~,3@e" 0.0) "+0.000E+0")
(test '("~,0e" 3.141) "3.E+0")
(test '("~,3,,0e" 3.14159) "0.314E+1")
(test '("~,5,3,-2e" 3.14159) "0.00314E+003")
(test '("~,5,3,-5e" -3.14159) "-0.00000E+006")
(test '("~,5,2,2e" 3.14159) "31.4159E-01")
(test '("~,5,2,,,,'ee" 0.0) "0.00000e+00")
(test '("~12,3e" -3.141) "   -3.141E+0")
(test '("~12,3,,,,'#e" -3.141) "###-3.141E+0")
(test '("~10,2e" -1.236e-4) "  -1.24E-4")
(test '("~5,3e" -3.141) "-3.141E+0")
(test '("~5,3,,,'*e" -3.141) "*****")
(test '("~3e" 3.14159) "3.14159E+0")
(test '("~4e" 3.14159) "3.14159E+0")
(test '("~5e" 3.14159) "3.14159E+0")
(test '("~5,,,,'*e" 3.14159) "*****")
(test '("~6e" 3.14159) "3.1E+0")
(test '("~7e" 3.14159) "3.14E+0")
(test '("~7e" -3.14159) "-3.1E+0")
(test '("~8e" 3.14159) "3.142E+0")
(test '("~9e" 3.14159) "3.1416E+0")
(test '("~9,,,,,,'ee" 3.14159) "3.1416e+0")
(test '("~10e" 3.14159) "3.14159E+0")
(test '("~11e" 3.14159) " 3.14159E+0")
(test '("~12e" 3.14159) "  3.14159E+0")
(test '("~13,6,2,-5e" 3.14159) " 0.000003E+06")
(test '("~13,6,2,-4e" 3.14159) " 0.000031E+05")
(test '("~13,6,2,-3e" 3.14159) " 0.000314E+04")
(test '("~13,6,2,-2e" 3.14159) " 0.003142E+03")
(test '("~13,6,2,-1e" 3.14159) " 0.031416E+02")
(test '("~13,6,2,0e" 3.14159)  " 0.314159E+01")
(test '("~13,6,2,1e" 3.14159)  " 3.141590E+00")
(test '("~13,6,2,2e" 3.14159)  " 31.41590E-01")
(test '("~13,6,2,3e" 3.14159)  " 314.1590E-02")
(test '("~13,6,2,4e" 3.14159)  " 3141.590E-03")
(test '("~13,6,2,5e" 3.14159)  " 31415.90E-04")
(test '("~13,6,2,6e" 3.14159)  " 314159.0E-05")
(test '("~13,6,2,7e" 3.14159)  " 3141590.E-06")
(test '("~13,6,2,8e" 3.14159)  "31415900.E-07")
(test '("~7,3,,-2e" 0.001) ".001E+0")
(test '("~8,3,,-2@e" 0.001) "+.001E+0")
(test '("~8,3,,-2@e" -0.001) "-.001E+0")
(test '("~8,3,,-2e" 0.001) "0.001E+0")
(test '("~7,,,-2e" 0.001) ".001E+0")
(test '("~12,3,1e" 3.14159e12) "   3.142E+12")
(test '("~12,3,1,,'*e" 3.14159e12) "************")
(test '("~5,3,1e" 3.14159e12) "3.142E+12")

; general floating point

(test '("~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
	0.0314159 0.0314159 0.0314159 0.0314159)
      "  3.14E-2|314.2$-04|0.314E-01|  3.14E-2")
(test '("~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
	0.314159 0.314159 0.314159 0.314159)
      "  0.31   |0.314    |0.314    | 0.31    ")
(test '("~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
	3.14159 3.14159 3.14159 3.14159)
      "   3.1   | 3.14    | 3.14    |  3.1    ")
(test '("~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
	31.4159 31.4159 31.4159 31.4159)
      "   31.   | 31.4    | 31.4    |  31.    ")
(test '("~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
	314.159 314.159 314.159 314.159)
      "  3.14E+2| 314.    | 314.    |  3.14E+2") 
(test '("~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
	3141.59 3141.59 3141.59 3141.59)
      "  3.14E+3|314.2$+01|0.314E+04|  3.14E+3")
(test '("~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
	3.14E12 3.14E12 3.14E12 3.14E12)
      "*********|314.0$+10|0.314E+13| 3.14E+12")
(test '("~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
	3.14E120 3.14E120 3.14E120 3.14E120)
      "*********|?????????|%%%%%%%%%|3.14E+120")
(test '("~g" 0.0) "0.0    ")

; dollar floating point

(test '("~$" 1.23) "1.23")
(test '("~$" 1.2) "1.20")
(test '("~$" 0.0) "0.00")
(test '("~$" 9.999) "10.00")
(test '("~3$" 9.9999) "10.000")
(test '("~,4$" 3.2) "0003.20")
(test '("~,4$" 10000.2) "10000.20")
(test '("~,4,10$" 3.2) "   0003.20")
(test '("~,4,10@$" 3.2) "  +0003.20")
(test '("~,4,10:@$" 3.2) "+  0003.20")
(test '("~,4,10:$" -3.2) "-  0003.20")
(test '("~,4,10$" -3.2) "  -0003.20")
(test '("~,,10@$" 3.2) "     +3.20")
(test '("~,,10:@$" 3.2) "+     3.20")
(test '("~,,10:@$" -3.2) "-     3.20")
(test '("~,,10,'_@$" 3.2) "_____+3.20")
(test '("~,,4$" 1234.4) "1234.40")

(if (not show-test) (display "done."))

(format #t "~%~a Test~:p completed. (~a failure~:p)~2%" total fails)

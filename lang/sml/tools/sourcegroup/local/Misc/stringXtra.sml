(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Gene Rollins (rollins+@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ, Pittsburgh, PA *)

structure StringXtra :STRINGXTRA = struct

fun isBlankChr (s :string) :bool =
  ((s=" ") orelse (s="\t") orelse (s="\n") orelse (s="\127") orelse (s="\012"))

fun isIdChr (s :string) :bool =
  let val i = ord s in
    (s="'") orelse (s="_") orelse
    (((ord "A") <= i) andalso (i <= (ord "Z"))) orelse
    (((ord "a") <= i) andalso (i <= (ord "z"))) orelse
    (((ord "0") <= i) andalso (i <= (ord "9")))
  end

fun skipBlanks ((i,s) :int * string) :int =
  let val len = String.size s
      fun skip j =
        if j = len
	  then j
	  else if isBlankChr (chr(ordof(s,j)))
	         then skip (j+1)
		 else j
  in skip i end

fun getWord ((i,s) :int * string) :int * string =
  let val len = String.size s 
      fun grab index =
	if index = len then index
	  else if isBlankChr (chr(ordof(s,index)))
	         then index
		 else grab (index+1)
      val index = grab i
  in (index, substring (s, i, index-i)) end

exception getIntError;
fun getInt ((i,s) :int * string) :int * int =
  let val len = String.size s 
      fun convert (a as (index, num)) =
        if index = len then a
	  else let val digit = ordof(s,index)-48 in
	         if (digit < 0) orelse (9 < digit) then a
	           else convert (index+1, num*10+digit)
	       end
      val (index, result) = convert (i, 0)
  in if index = i then raise getIntError
       else (index, result)
  end

fun findChr (ch :string) ((i,s) :int * string) :int =
  let val len = String.length s
      fun find j =
        if j=len
	  then 0
	  else if ch = chr(ordof(s,j))
	         then j+1
		 else find (j+1)
  in if (size ch) = 0 then 0 else find i end

fun findChrFromRight (ch :string) ((i,s) :int * string) :int =
  let val len = String.length s
      fun find j =
        if j = ~1
	  then len
	  else if ch = chr(ordof(s,j))
	         then j
		 else find (j-1)
  in if (size ch) = 0 then len else find (i-1) end

fun stringListEqual (a :string list) (b :string list) :bool =
  case (a, b) of
     ([], []) => true
   | (head::tail, []) => false
   | ([], head::tail) => false
   | (a1::ax, b1::bx) =>
       if a1 = b1
         then stringListEqual ax bx
         else false

fun printl (lst :string list) :unit = (map print lst; ())

fun printSep ([] :string list) (sep :string) :unit = ()
  | printSep (a::[]) sep = print a
  | printSep (a::(rest as (b::c))) sep =
      (print a; print sep; printSep rest sep)

fun stringListPrint lst sep =
  (print "["; printSep lst sep; print "]")

fun stringEqual (x:string, y:string) = (x=y)

fun breakAtBlanks (line:string) :string list =
  let fun process (s:string) (accum:string list) :string list =
        let val len = size s
            val pos = skipBlanks (0, s)
        in
          if pos = len then accum else
            let val (next'pos, word) = getWord (pos, s)
                val next'part = substring (s, next'pos, (size s) - next'pos)
            in process next'part (word::accum) end
        end
  in process line [] end

end

(* Copyright (c) 1992 by Carnegie Mellon University *)

(* Eric Cooper (eric.cooper@cs.cmu.edu)
   School of Computer Science, Carnegie-Mellon Univ., Pittsburgh, PA 15213 *)

structure StringFns :STRING_FNS =
    struct
	fun char c =
	    if c = "\n" then
		"\\n"
	    else if c = "\t" then
		"\\t"
	    else if c = "\"" orelse c = "\\" then
		"\\" ^ c
	    else
		let val n = ord c
		in
		    if ord " " <= n andalso n <= ord "~" then
			c
		    else
			"\\" ^
			let val s = makestring n
			in
			    if n < 10 then
				"00" ^ s
			    else if n < 100 then
				"0" ^ s
			    else
				s
			end
		end

	fun escape s = "\"" ^ implode (map char (explode s)) ^ "\""

	fun increment "" = "a"
	  | increment str =
	    let val n = size str
		val prefix = substring (str, 0, n-1)
		val lastc = substring (str, n-1, 1)
	    in
		if lastc = "z" then
		    (increment prefix) ^ "a"
		else
		    prefix ^ chr (ord lastc + 1)
	    end

	val counter = ref ""

	fun gensym prefix =
	    let val s = increment (!counter)
	    in
		counter := s;
		prefix ^ s
	    end
    end

(* Copyright 1989 by AT&T Bell Laboratories *)
(* basics/pputil.sml *)

structure PPUtil : PPUTIL =
struct

  structure Symbol : SYMBOL = Symbol
  structure PP = PrettyPrint

  fun ppSequence0 ppstream (sep:PP.ppstream->unit,pr,elems) =
      let fun prElems [el] = pr ppstream el
	    | prElems (el::rest) =
	        (pr ppstream el;
		 sep ppstream;
                 prElems rest)
	    | prElems [] = ()
       in prElems elems
      end

  fun ppSequence ppstream {sep:PP.ppstream->unit, pr:PP.ppstream->'a->unit, 
                           style:PP.break_style} (elems: 'a list) =
      (PP.begin_block ppstream style 0;
       ppSequence0 ppstream (sep,pr,elems);
       PP.end_block ppstream)

  fun ppClosedSequence ppstream{front:PP.ppstream->unit,sep:PP.ppstream->unit,
                               back:PP.ppstream->unit,pr:PP.ppstream->'a->unit,
                                style:PP.break_style} (elems:'a list) =
      (PP.begin_block ppstream PP.CONSISTENT 0;
       front ppstream;
       PP.begin_block ppstream style 0;
       ppSequence0 ppstream (sep,pr,elems); 
       PP.end_block ppstream;
       back ppstream;
       PP.end_block ppstream)

  fun ppSym ppstream (s:Symbol.symbol) = PP.add_string ppstream (Symbol.name s)

  fun formatQid p =
    let fun f [s] = [Symbol.name s]
          | f (a::r) = Symbol.name a :: "." :: f r
          | f nil = ["<bogus qid>"]
     in implode(f p)
    end

  val stringDepth = System.Print.stringDepth

  fun decimal i = let val m = Integer.makestring
		  in  m(i div 100)^m((i div 10)mod 10)^m(i mod 10) end
  val ctrl_a = 1
  val ctrl_z = 26
  val offset = ord "A" - ctrl_a
  val smallestprintable = ord " "
  val biggestprintable = ord "~"
  fun ml_char "\n" = "\\n"
    | ml_char "\t" = "\\t"
    | ml_char "\\" = "\\\\"
    | ml_char "\"" = "\\\""
    | ml_char c =
	  let val char = ord c
	  in  if char >= ctrl_a andalso char <= ctrl_z
	      then "\\^" ^ chr(char+offset)
	      else if char >= smallestprintable andalso char <= biggestprintable
		   then c
	      else "\\" ^ decimal char
	  end

  fun mlstr s = "\"" ^ implode(map ml_char (explode s)) ^ "\""
  fun pp_mlstr ppstream s =
      let val depth = !stringDepth
          val add_string = PP.add_string ppstream
	  fun pr i =
	      if i=depth then add_string "#"
	      else (let val ch = substring(s,i,1)
		    in  add_string (ml_char ch); pr (i+1)
		    end handle Substring => ())
      in add_string "\""; pr 0; add_string "\""
      end

  fun ppvseq ppstream ind (sep:string) pr elems =
      let fun prElems [el] = pr ppstream el
	    | prElems (el::rest) = (pr ppstream el; 
                                    PP.add_string ppstream sep; 
                                    PP.add_newline ppstream;
                                    prElems rest)
	    | prElems [] = ()
       in PP.begin_block ppstream PP.CONSISTENT ind;
          prElems elems;
          PP.end_block ppstream
      end

  (* debug print functions *)
  fun ppIntPath ppstream =
        ppClosedSequence ppstream 
	  {front=(fn pps => PP.add_string pps "["),
	   sep=(fn pps => (PP.add_string pps ","; PP.add_break pps (0,0))),
	   back=(fn pps => PP.add_string pps "]"),
	   style=PP.INCONSISTENT,
	   pr=(fn pps => PP.add_string pps o (makestring:int->string))}
  fun ppSymPath ppstream = 
     ppSequence ppstream {sep=(fn pps => PP.add_string pps "."),
			  style=PP.INCONSISTENT,
			  pr=ppSym}

end (* structure PPUtil *)

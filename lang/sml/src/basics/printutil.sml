(* Copyright 1989 by AT&T Bell Laboratories *)
structure PrintUtil : PRINTUTIL = struct

  val say = System.Print.say

  structure Symbol : SYMBOL = Symbol

  fun newline () = say "\n"
  fun tab 0 = () | tab n = (say " "; tab(n-1))

  fun printSequence (separator: string) pr elems =
      let fun prElems [el] = pr el
	    | prElems (el::rest) = (pr el; say separator; prElems rest)
	    | prElems [] = ()
       in prElems elems
      end

  fun printClosedSequence (front: string, sep, back:string) pr elems =
      (say front; printSequence sep pr elems; say back)

  fun printSym(s: Symbol.symbol) = print(Symbol.name s)
      (* fix -- maybe this belongs in Symbol *)

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
  fun pr_mlstr s =
      let val depth = !stringDepth
	  fun pr i =
	      if i=depth then say "#"
	      else (let val ch = substring(s,i,1)
		    in  print(ml_char ch); pr (i+1)
		    end handle Substring => ())
      in say "\""; pr 0; say "\""
      end

  fun nlindent n = (newline(); tab n)

  fun printvseq ind (sep:string) pr elems =
      let fun prElems [el] = pr el
	    | prElems (el::rest) = (pr el; nlindent ind; say sep; prElems rest)
	    | prElems [] = ()
       in prElems elems
      end

  (* debug print functions *)
  val prIntPath = printClosedSequence ("[",",","]") (say o Integer.makestring)
  val prSymPath = printSequence "." printSym

end (* structure PrintUtil *)

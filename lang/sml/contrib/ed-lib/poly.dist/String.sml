(*$String : STRING General StringType List *)

loadSig "STRING";

structure String: STRING =

(* ASCII STRINGS

Created by:	Dave Berry, LFCS, University of Edinburgh
                db@lfcs.ed.ac.uk
Date:	        17 Sep 1991

Maintenance:	Author

DESCRIPTION

   Poly/ML provides a substring operation similar to extract.


SEE ALSO

   AsciiOrdString.


RCS LOG

$Log: String.sml,v $
Revision 1.1  1994/02/08 00:23:15  jkh
Initial revision

# Revision 1.1  1991/10/22  17:54:43  db
# Initial revision
#


*)

struct


(* PERVASIVES *)

  type string = string

  exception Chr = Chr
  and Ord = Ord

  val size = size
  val ord = ord
  val chr = chr
  val explode = explode
  val implode = implode
  val op ^ = op ^


(* TYPES *)

  type T = string

  datatype Mode = IgnoreCase | MatchCase



(* CREATORS *)

  exception Size of string * int

  fun create n s =
	if n < 0 then raise Size ("create", n)
	else General.iterate n (fn x => s ^ x) ""


(* CONVERTORS *)

  local
    fun show' h =
        case h
        of "\n"   => ["\\n"]
        |  "\t"   => ["\\t"]
        |  " "    => [" "]
        |  "\\"   => ["\\\\"]
        |  "\""   => ["\\\""]
        |  "\127" => ["\\127"]
        |  _ =>
            if StringType.isVisible h then [h]
            else if StringType.isControl h then
              ["\\^", chr (ord h + ord "@")]
            else
              let val i = ord h
                  val s = CoreUtils.intToString i
                  val s' = if i < 10 then "00" ^ s
                           else if i < 100 then "0" ^ s
                           else s
              in ["\\", s]
              end

    fun show nil = nil
    |   show (h::t) = show' h @ show t
  in
    fun string s = "\"" ^ implode (show (explode s)) ^ "\""
  end

  fun print os s = output (os, string s)


(* ITERATORS *)

  fun map f s = implode (List.map f (explode s))

  fun apply f s = List.apply f (explode s)

  fun mapAll p f s = implode (List.mapAll p f (explode s))

  fun applyAll p f s = (List.applyAll p f (explode s))


(* SELECTORS *)

  exception Subscript of string * int

  exception Extract of int * int

  fun extract start finish s =
        PolyML.StringBuiltIns.substring (s, start + 1, finish - start)
        handle PolyML.StringBuiltIns.Substring => raise Extract (start, finish)


(* SOME MANIPULATORS *)

  exception Empty of string

  fun upper s =
        if StringType.isLower s
        then chr (ord s + ord "A" - ord "a") ^ extract 1 (size s) s
        else s
        handle StringType.Empty _ => raise Empty "upper"

  fun lower s =
        if StringType.isUpper s
        then chr (ord s + ord "a" - ord "A") ^ extract 1 (size s) s
        else s
        handle StringType.Empty _ => raise Empty "lower"

  fun ascii s =
        if StringType.isAscii s then s
        else chr (ord s - 128) ^ extract 1 (size s) s
        handle StringType.Empty _ => raise Empty "ascii"

  fun control s =
        let val s' = chr (ord s - 64)
        in if StringType.isControl s'
           then s' ^ extract 1 (size s) s
           else s
        end
        handle Chr => s
        |      StringType.Empty _ => raise Empty "control"


(* LOCAL *)

  fun ignoreCase p =
	fn x => p (lower x) orelse p (upper x)
   (* ignoreCase p; returns a predicate based on p that ignores the case
      of its argument. *)

  fun dropPrefix p s = implode (List.dropPrefix p (explode s))


(* OBSERVERS *)

  fun forAll MatchCase p s = List.forAll p (explode s)
  |   forAll IgnoreCase p s = List.forAll (ignoreCase p) (explode s)

  fun exists MatchCase p s = List.exists p (explode s)
  |   exists IgnoreCase p s = List.exists (ignoreCase p) (explode s)

  fun prefixes MatchCase s1 s2 n =
      ( List.prefixes (explode s1) (explode (extract n (size s2) s2))
	handle Extract _ => raise Subscript ("prefixes", n)
      )
  |   prefixes IgnoreCase s1 s2 n =
      ( List.prefixes (List.map lower (explode s1))
			    (List.map lower (explode (extract n (size s2) s2)))
	handle Extract _ => raise Subscript ("prefixes", n)
      )

  fun postfixes MatchCase s1 s2 n =
      ( List.prefixes (rev (explode s1))
			      (rev (explode (extract 0 n s2)))
	handle Extract _ => raise Subscript ("postfixes", n)
      )
  |   postfixes IgnoreCase s1 s2 n =
      ( List.prefixes
	    (List.map lower (rev (explode s1)))
	    (List.map lower (rev (explode (extract n (size s2) s2))))
	handle Extract _ => raise Subscript ("prefixes", n)
      )

  fun eqMode MatchCase s s' = (s = s')
  |   eqMode IgnoreCase s s' = (map lower s = map lower s')

  fun neMode MatchCase s s' = (s <> s')
  |   neMode IgnoreCase s s' = (map lower s <> map lower s')

  val eq = eqMode MatchCase

  val ne = neMode MatchCase

  val fixedWidth = false


(* MANIPULATING THE NTH ELEMENT *)

  infix 9 sub
  fun s sub n = chr (PolyML.StringBuiltIns.ordof (s, n+1))
		handle PolyML.StringBuiltIns.Ordof =>
			 raise Subscript ("sub", n)

  fun nth n s = s sub n
		handle Subscript _ => raise Subscript ("nth", n)


(* SEARCHING AND INDEXING *)

  local
    fun search' _ nil _ = Fail ()
    |   search' s (s' as _::t') n =
      if List.prefixes s s' then OK n
      else search' s t' (n+1)	(* Boyer and Moore?  Never heard of them! *)
  in
    fun search mode s' s n =
	  if n < 0 orelse n >= size s then raise Subscript ("search", n)
	  else if s' = "" then OK n
	  else if mode = MatchCase then
	    search' (explode s') (explode (extract n (size s) s)) n
	  else
	    search' (List.map lower (explode s'))
		    (List.map lower (explode (extract n (size s) s))) n
    fun revSearch mode s' s n =
	  if n < 0 orelse n > size s then raise Subscript ("revSearch", n)
	  else if s' = "" then OK (size s - 1)
	  else 
	  let val s1' = (List.map lower (List.rev (explode s')))
	      val s1  = (List.map lower (List.rev (explode (extract 0 n s))))
	  in case search' s1' s1 0 of
	       (OK i) => OK (n - i - size s')
	     |  Fail () => Fail ()
	  end
  end

  fun occurs mode s' s n =
	case search mode s' s n 
	of OK _ => true
	|  Fail () => false
	handle Subscript _ => raise Subscript ("occurs", n)

  fun revOccurs mode s' s n =
	case revSearch mode s' s n 
	of OK _ => true
	|  Fail () => false
	handle Subscript _ => raise Subscript ("revOccurs", n)

  fun index MatchCase p s n =
      (( case List.index p (explode (extract n (size s) s)) of
	    OK i => OK (i + n)
	  | x => x
       )
       handle Extract _ => raise Subscript ("index", n)
      )
  |   index IgnoreCase p s n =
      (( case List.index (ignoreCase p) (explode (extract n (size s) s)) of
	    OK i => OK (i + n)
	  | x => x
       )
       handle Extract _ => raise Subscript ("index", n)
      )

  fun revIndex MatchCase p s n =
      (( case List.index p (rev (explode (extract 0 n s))) of
	    OK i => OK (n - i - 1)
	  | x => x
       )
       handle Extract _ => raise Subscript ("revIndex", n)
      )
  |   revIndex IgnoreCase p s n =
      (( case List.index (ignoreCase p) (rev (explode (extract 0 n s))) of
	    OK i => OK (n - i - 1)
	  | x => x
       )
       handle Extract _ => raise Subscript ("revIndex", n)
      )


(* OTHER MANIPULATORS *)

  exception Char of string * string

  fun skipSpaces s = dropPrefix (not o StringType.isVisible) s

  fun subst MatchCase c s' s =
	if size c <> 1 then raise Char ("subst", c)
	else implode (List.updateAll (eqMode MatchCase c) s' (explode s))
  |   subst IgnoreCase c s' s =
	if size c <> 1 then raise Char ("subst", c)
	else
	  let fun changeFn ch =
		    if StringType.isLower ch then map lower s'
		    else if StringType.isUpper ch then map upper s'
		    else s'
	  in implode (List.changeAll (eqMode IgnoreCase c) changeFn (explode s))
	  end

  fun showAscii s =
	let val s' = string s
	in extract 1 (size s') s'
	end

  fun rev s = implode (List.rev (explode s))

  fun padL c w s =
	if size c <> 1 then raise Char ("padL", c)
	else if size s >= w then s
	else (create (w - size s) c) ^ s

  fun padR c w s =
	if size c <> 1 then raise Char ("padL", c)
	else if size s >= w then s
	else s ^ (create (w - size s) c)

  fun padC c w s =
	if size c <> 1 then raise Char ("padL", c)
	else if size s >= w then s
	else
	  let val n = w - size s
	      val l = n div 2
	      val r = if n mod 2 = 0 then n div 2 else n div 2 + 1
	  in (create l c) ^ s ^ (create r c)
	  end

  fun truncL w s =
	if size s <= w then s
	else extract (size s - w) (size s) s

  fun truncR w s =
	if size s <= w then s
	else extract 0 w s

  fun truncC w s =
	if size s <= w then s
	else
	  let val n = size s - w
	      val r = n div 2
	      val l = if n mod 2 = 0 then n div 2 else n div 2 + 1
	  in extract l r s
	  end

  fun dropL c s =
	if size c <> 1 then raise Char ("dropL", c)
	else dropPrefix (fn x => (x = c)) s

  fun dropR c s =
	if size c <> 1 then raise Char ("dropR", c)
	else rev (dropPrefix (fn x => (x = c)) (rev s))

end
